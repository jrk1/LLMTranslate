item_server <- function(input, output, session, file_rv, rv) {
  output$item_progress_ui <- renderUI(render_progress_ui(rv))
  output$item_preview_ui <- renderUI(render_preview_ui(rv))

  bind_prompt_preview(output, input, "item_fwd_prompt_preview", DEFAULT_FORWARD)
  bind_prompt_preview(output, input, "item_back_prompt_preview", DEFAULT_BACK)
  bind_prompt_preview(output, input, "item_recon_prompt_preview", DEFAULT_RECON_UI)

  observeEvent(input$item_do_back, {
    if (!isTRUE(input$item_do_back)) {
      updateCheckboxInput(session, "item_do_recon", value = FALSE)
    }
  })

  observeEvent(input$item_stop, {
    rv$stop <- TRUE
    update_status(rv, "STOP requested\u2026 finishing current call.", "warning", session)
  })

  observeEvent(input$item_reset, {
    reset_mode_rv(session, rv)
  })

  output$item_download_ui <- renderUI({
    req(rv$result)
    div(class = "download-btn", downloadButton("item_download", "Download Excel"))
  })

  output$item_download <- downloadHandler(
    filename = function() paste0("translated_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(rv$result)
      logs <- build_download_content(
        input, rv, "Item-by-item Translation",
        DEFAULT_FORWARD, DEFAULT_BACK, DEFAULT_RECON_UI
      )
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Translation Results")
      openxlsx::writeData(wb, "Translation Results", rv$result)
      openxlsx::addWorksheet(wb, "Model Selection Log")
      openxlsx::writeData(wb, "Model Selection Log", logs$model_log)
      openxlsx::addWorksheet(wb, "Prompt Log")
      openxlsx::writeData(wb, "Prompt Log", logs$prompt_log)
      if (length(rv$log) > 0) {
        debug_df <- data.frame(
          Timestamp = sub(" \\| .*", "", rv$log),
          Message = sub("^[^ ]+ \\| ", "", rv$log),
          stringsAsFactors = FALSE
        )
        openxlsx::addWorksheet(wb, "Debug Log")
        openxlsx::writeData(wb, "Debug Log", debug_df)
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$item_debug_log <- renderText({
    if (isTRUE(input$item_debug)) paste(rv$log, collapse = "\n") else ""
  })

  output$item_table <- renderDT({
    dat <- if (is.null(rv$result)) file_rv$df else rv$result
    req(dat)
    datatable(dat, options = list(pageLength = 20, scrollX = TRUE))
  })

  observeEvent(input$item_run, {
    if (!validate_before_run(file_rv, input, "orig_col")) return()
    if (rv$running) {
      showNotification("Translation already running!", type = "warning", duration = 3)
      return()
    }

    items_col <- as.character(file_rv$df[[input$orig_col]])
    n_non_empty <- sum(!is.na(items_col) & nzchar(trimws(items_col)))
    if (n_non_empty >= 500 && (is.null(rv$state) || rv$state$i == 1)) {
      showNotification(
        paste0("Processing ", n_non_empty, " items. This may take some time."),
        type = "warning", duration = 10
      )
    }

    resume <- !is.null(rv$state) && rv$state$i <= rv$state$n
    if (!resume) {
      rv$stop <- FALSE; rv$running <- TRUE
      rv$log <- character(); rv$preview <- NULL
    } else {
      rv$stop <- FALSE; rv$running <- TRUE
    }

    logger <- make_app_logger(rv, enabled = TRUE)
    logger(if (resume) "---- RUN RESUME ----" else "---- RUN START ----")

    f_model <- full_model(input, "forward_provider", "forward_model")
    b_model <- full_model(input, "back_provider", "back_model")
    r_model <- full_model(input, "recon_provider", "recon_model")

    if (!resume) {
      from_lang <- input$lang_from
      to_lang <- input$lang_to
      cfg <- list(
        from_lang = from_lang,
        to_lang = to_lang,
        context = .pkg$format_context(input$instrument_context),
        register = .pkg$format_register(input$target_register),
        forward_prompt = DEFAULT_FORWARD,
        back_prompt = DEFAULT_BACK,
        recon_prompt = DEFAULT_RECON_UI,
        do_back = isTRUE(input$item_do_back),
        do_recon = isTRUE(input$item_do_back) && isTRUE(input$item_do_recon),
        f_temp = input$forward_temp,
        b_temp = input$back_temp,
        r_temp = input$recon_temp,
        f_model = f_model, b_model = b_model, r_model = r_model,
        orig_col = input$orig_col,
        df = file_rv$df,
        n = nrow(file_rv$df),
        n_non_empty = sum(
          !is.na(file_rv$df[[input$orig_col]]) &
            nzchar(trimws(as.character(file_rv$df[[input$orig_col]])))
        ),
        items_done = 0L,
        i = 1,
        stage = "fwd",
        forward_out = NULL,
        back_out = NULL
      )
      item_cols <- .pkg$init_columns(
        cfg$df, from_lang, to_lang, cfg$do_back, cfg$do_recon
      )
      cfg$df <- item_cols$data
      cfg$fwd_col <- item_cols$fwd_col
      cfg$back_col <- item_cols$back_col
      cfg$recon_col <- item_cols$recon_col
      cfg$change_col <- item_cols$change_col
      cfg$severity_col <- item_cols$severity_col

      rv$state <- cfg
      update_status(rv, "Starting\u2026", "message", session)
    } else {
      update_status(rv, "Resuming\u2026", "message", session)
    }

    process_step <- function() {
      cfg <- isolate(rv$state)
      if (is.null(cfg)) return()

      if (isolate(rv$stop)) {
        rv$running <- FALSE; rv$result <- cfg$df
        update_status(rv, "Stopped. Press Start to resume.", "warning", session)
        logger("---- RUN STOPPED ----")
        return()
      }

      if (cfg$i > cfg$n) {
        rv$running <- FALSE; rv$result <- cfg$df
        update_status(rv, "Done.", "message", session)
        logger("---- RUN END ----")
        isolate(rv$state <- NULL)
        return()
      }

      original_text <- as.character(cfg$df[[cfg$orig_col]][cfg$i])
      if (is.na(original_text) || !nzchar(original_text)) {
        logger("Item", cfg$i, "empty; skipping")
        cfg$i <- cfg$i + 1; cfg$stage <- "fwd"
        rv$state <- cfg
        later(process_step, 0.01)
        return()
      }

      stage_label <- switch(
        cfg$stage,
        fwd = "\u2192 forward translating",
        back = "\u2190 back translating",
        recon = "\u21bb reconciling",
        "next_item" = "done"
      )
      update_status(
        rv, sprintf("%s | Item %d/%d", stage_label, cfg$items_done + 1L, cfg$n_non_empty),
        "message", session
      )
      logger("Processing item", cfg$i, "stage", cfg$stage)

      isolate({
        rv$progress_pct <- round(cfg$items_done / max(cfg$n_non_empty, 1L) * 100)
        rv$progress_text <- sprintf(
          "Item %d of %d (%d%%)", cfg$items_done + 1L, cfg$n_non_empty, rv$progress_pct
        )
      })

      tryCatch({
        switch(
          cfg$stage,
          fwd = {
            out <- .pkg$translate_text(
              original_text, cfg$f_model, cfg$from_lang, cfg$to_lang,
              cfg$context, cfg$register, cfg$f_temp, cfg$forward_prompt,
              logger
            )
            cfg$df[[cfg$fwd_col]][cfg$i] <- out
            cfg$forward_out <- out
            cfg$stage <- if (cfg$do_back) "back" else "next_item"
          },
          back = {
            out <- .pkg$translate_text(
              cfg$forward_out, cfg$b_model, cfg$from_lang, cfg$to_lang,
              cfg$context, cfg$register, cfg$b_temp, cfg$back_prompt,
              logger
            )
            cfg$df[[cfg$back_col]][cfg$i] <- out
            cfg$back_out <- out
            cfg$stage <- if (cfg$do_recon) "recon" else "next_item"
          },
          recon = {
            parsed <- .pkg$reconcile_one(
              original_text, cfg$forward_out, cfg$back_out,
              cfg$r_model, cfg$from_lang, cfg$to_lang,
              cfg$context, cfg$register, cfg$r_temp,
              cfg$recon_prompt, logger
            )
            cfg$df[[cfg$recon_col]][cfg$i] <- parsed$revised
            cfg$df[[cfg$change_col]][cfg$i] <- parsed$explanation
            if (!is.null(cfg$severity_col)) {
              cfg$df[[cfg$severity_col]][cfg$i] <- parsed$severity %||% NA_character_
            }
            cfg$stage <- "next_item"
          },
          next_item = {
            if (is.null(isolate(rv$preview))) {
              isolate({
                rv$preview <- list(
                  original = original_text,
                  forward = cfg$df[[cfg$fwd_col]][cfg$i],
                  back = if (!is.null(cfg$back_col)) cfg$df[[cfg$back_col]][cfg$i] else NULL,
                  reconciled = if (!is.null(cfg$recon_col)) cfg$df[[cfg$recon_col]][cfg$i] else NULL
                )
              })
            }
            cfg$items_done <- cfg$items_done + 1L
            cfg$i <- cfg$i + 1; cfg$stage <- "fwd"
          }
        )

        rv$state <- cfg
        later(process_step, 0.01)

      }, error = function(e) {
        logger("ERROR:", e$message)
        err_msg <- paste0("ERROR: ", e$message)
        switch(
          cfg$stage,
          fwd = { cfg$df[[cfg$fwd_col]][cfg$i] <- err_msg },
          back = { cfg$df[[cfg$back_col]][cfg$i] <- err_msg },
          recon = {
            cfg$df[[cfg$recon_col]][cfg$i] <- err_msg
            cfg$df[[cfg$change_col]][cfg$i] <- ""
            if (!is.null(cfg$severity_col)) {
              cfg$df[[cfg$severity_col]][cfg$i] <- NA_character_
            }
          }
        )
        cfg$stage <- "next_item"
        rv$state <- cfg
        later(process_step, 0.01)
      })
    }

    later(process_step, 0.01)
  })
}
