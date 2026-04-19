batch_server <- function(input, output, session, file_rv, batch_rv) {
  output$batch_progress_ui <- renderUI(render_progress_ui(batch_rv))
  output$batch_preview_ui <- renderUI(render_preview_ui(
    batch_rv, "Batch Translation Preview (first item shown - verify quality)"
  ))

  bind_prompt_preview(output, input, "batch_fwd_prompt_preview", DEFAULT_BATCH_FORWARD)
  bind_prompt_preview(output, input, "batch_back_prompt_preview", DEFAULT_BATCH_BACK)
  bind_prompt_preview(output, input, "batch_recon_prompt_preview", DEFAULT_BATCH_RECON)

  observeEvent(input$batch_do_back, {
    if (!isTRUE(input$batch_do_back)) {
      updateCheckboxInput(session, "batch_do_recon", value = FALSE)
    }
  })

  observeEvent(input$batch_stop, {
    batch_rv$stop <- TRUE
    update_status(batch_rv, "STOP requested\u2026", "warning", session)
  })

  observeEvent(input$batch_reset, {
    reset_mode_rv(session, batch_rv)
  })

  output$batch_download_ui <- renderUI({
    req(batch_rv$result)
    div(class = "download-btn", downloadButton("batch_download", "Download Excel"))
  })

  output$batch_download <- downloadHandler(
    filename = function() paste0("batch_translated_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(batch_rv$result)
      logs <- build_download_content(
        input, batch_rv, "Batch Translation",
        DEFAULT_BATCH_FORWARD, DEFAULT_BATCH_BACK, DEFAULT_BATCH_RECON
      )
      wb <- openxlsx::createWorkbook()

      if (is.list(batch_rv$result) && !is.data.frame(batch_rv$result)) {
        for (sheet_name in names(batch_rv$result)) {
          openxlsx::addWorksheet(wb, sheet_name)
          openxlsx::writeData(wb, sheet_name, batch_rv$result[[sheet_name]])
        }
      } else {
        cfg <- batch_rv$state
        sheet_name <- if (!is.null(cfg$sheet_name)) cfg$sheet_name else "Batch Translation Results"
        openxlsx::addWorksheet(wb, sheet_name)
        openxlsx::writeData(wb, sheet_name, batch_rv$result)
      }

      openxlsx::addWorksheet(wb, "Model Selection Log")
      openxlsx::writeData(wb, "Model Selection Log", logs$model_log)
      openxlsx::addWorksheet(wb, "Prompt Log")
      openxlsx::writeData(wb, "Prompt Log", logs$prompt_log)
      if (length(batch_rv$log) > 0) {
        debug_df <- data.frame(
          Timestamp = sub(" \\| .*", "", batch_rv$log),
          Message = sub("^[^ ]+ \\| ", "", batch_rv$log),
          stringsAsFactors = FALSE
        )
        openxlsx::addWorksheet(wb, "Debug Log")
        openxlsx::writeData(wb, "Debug Log", debug_df)
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$batch_debug_log <- renderText({
    if (isTRUE(input$batch_debug)) paste(batch_rv$log, collapse = "\n") else ""
  })

  output$batch_sheet_display_selector <- renderUI({
    req(batch_rv$result)
    if (is.list(batch_rv$result) && !is.data.frame(batch_rv$result)) {
      sheet_names <- names(batch_rv$result)
      selectInput(
        "batch_display_sheet", "Select sheet to display:",
        choices = sheet_names, selected = sheet_names[1], width = "300px"
      )
    }
  })

  output$batch_table <- renderDT({
    if (is.null(batch_rv$result)) {
      dat <- file_rv$df
    } else {
      if (is.list(batch_rv$result) && !is.data.frame(batch_rv$result)) {
        selected_display_sheet <- input$batch_display_sheet
        if (is.null(selected_display_sheet) ||
            !selected_display_sheet %in% names(batch_rv$result)) {
          selected_display_sheet <- names(batch_rv$result)[1]
        }
        dat <- batch_rv$result[[selected_display_sheet]]
      } else {
        dat <- batch_rv$result
      }
    }
    req(dat)
    datatable(dat, options = list(pageLength = 20, scrollX = TRUE))
  })

  # ---- Non-blocking batch translation via later() state machine ----

  observeEvent(input$batch_run, {
    if (!validate_before_run(file_rv, input, "orig_col")) return()
    if (batch_rv$running) {
      showNotification("Translation already running!", type = "warning", duration = 3)
      return()
    }

    batch_rv$stop <- FALSE; batch_rv$running <- TRUE
    batch_rv$log <- character(); batch_rv$preview <- NULL

    logger <- make_app_logger(batch_rv, enabled = TRUE)
    logger("---- BATCH TRANSLATION START ----")

    f_model <- full_model(input, "forward_provider", "forward_model")
    b_model <- full_model(input, "back_provider", "back_model")
    r_model <- full_model(input, "recon_provider", "recon_model")

    from_lang <- input$lang_from
    to_lang <- input$lang_to

    batch_context  <- .pkg$format_context(input$instrument_context)
    batch_register <- .pkg$format_register(input$target_register)
    batch_size <- if (is.na(input$batch_size) || is.null(input$batch_size)) NULL else as.integer(input$batch_size)

    selected_sheet <- file_rv$selected_sheet
    all_sheets <- file_rv$sheets %||% openxlsx::getSheetNames(file_rv$file_path)
    sheets_to_process <- if (identical(selected_sheet, "All sheets")) all_sheets else selected_sheet

    logger(paste(
      "Processing", length(sheets_to_process), "sheet(s):",
      paste(sheets_to_process, collapse = ", ")
    ))

    cfg <- list(
      sheets_to_process = sheets_to_process,
      sheet_idx = 1,
      stage = "read_sheet",
      from_lang = from_lang,
      to_lang = to_lang,
      forward_prompt = DEFAULT_BATCH_FORWARD,
      back_prompt = DEFAULT_BATCH_BACK,
      recon_prompt = DEFAULT_BATCH_RECON,
      do_back = isTRUE(input$batch_do_back),
      do_recon = isTRUE(input$batch_do_back) && isTRUE(input$batch_do_recon),
      f_model = f_model, b_model = b_model, r_model = r_model,
      f_temp = input$forward_temp, b_temp = input$back_temp, r_temp = input$recon_temp,
      batch_size = batch_size,
      orig_col = input$orig_col,
      context = batch_context,
      register = batch_register,
      file_path = file_rv$file_path,
      all_results = list(),
      failed_sheets = character(),
      df = NULL, fwd_col = NULL, back_col = NULL, recon_col = NULL,
      change_col = NULL, non_empty_idx = NULL, non_empty_items = NULL
    )

    batch_rv$state <- cfg

    process_batch_step <- function() {
      cfg <- isolate(batch_rv$state)
      if (is.null(cfg)) return()

      if (isolate(batch_rv$stop)) {
        batch_rv$running <- FALSE
        update_status(batch_rv, "Stopped.", "warning", session)
        logger("---- BATCH STOPPED ----")
        if (length(cfg$all_results) > 0) {
          if (length(cfg$all_results) == 1) {
            batch_rv$result <- cfg$all_results[[1]]
          } else {
            batch_rv$result <- cfg$all_results
          }
        }
        return()
      }

      tryCatch({
        switch(
          cfg$stage,

          read_sheet = {
            if (cfg$sheet_idx > length(cfg$sheets_to_process)) {
              cfg$stage <- "done"
              batch_rv$state <- cfg
              later(process_batch_step, 0.01)
              return()
            }

            current_sheet <- cfg$sheets_to_process[cfg$sheet_idx]
            logger(paste(
              "=== Processing sheet:", current_sheet, "(",
              cfg$sheet_idx, "of", length(cfg$sheets_to_process), ") ==="
            ))

            df <- openxlsx::read.xlsx(cfg$file_path, sheet = current_sheet)
            items <- df[[cfg$orig_col]]

            cols <- .pkg$init_columns(
              df, cfg$from_lang, cfg$to_lang, cfg$do_back, cfg$do_recon
            )
            df <- cols$data
            cfg$fwd_col <- cols$fwd_col
            cfg$back_col <- cols$back_col
            cfg$recon_col <- cols$recon_col
            cfg$change_col <- cols$change_col
            cfg$severity_col <- cols$severity_col

            non_empty_idx <- which(!is.na(items) & nzchar(trimws(as.character(items))))
            if (length(non_empty_idx) == 0) {
              logger(paste("Skipping sheet", current_sheet, "- no non-empty items"))
              cfg$all_results[[current_sheet]] <- df
              cfg$sheet_idx <- cfg$sheet_idx + 1
              batch_rv$state <- cfg
              later(process_batch_step, 0.01)
              return()
            }

            cfg$df <- df
            cfg$items <- as.character(items)
            cfg$non_empty_idx <- non_empty_idx
            cfg$non_empty_items <- items[non_empty_idx]
            cfg$current_sheet <- current_sheet

            logger(paste(
              "Total rows:", length(items),
              "| Non-empty items:", length(non_empty_idx)
            ))

            batch_rv$progress_pct <- 10
            batch_rv$progress_text <- "Preparing batch translation..."

            cfg$stage <- "fwd"
            batch_rv$state <- cfg
            later(process_batch_step, 0.01)
          },

          fwd = {
            sheet_status <- if (length(cfg$sheets_to_process) > 1) {
              paste0(" [Sheet: ", cfg$current_sheet, "]")
            } else ""
            update_status(
              batch_rv,
              paste0("\u2192 Forward translating batch...", sheet_status),
              "message", session
            )
            logger("Stage: Forward translation")

            batch_rv$progress_pct <- 20
            batch_rv$progress_text <- "Forward translating..."

            cfg$df[[cfg$fwd_col]][cfg$non_empty_idx] <- .pkg$batch_forward(
              as.character(cfg$non_empty_items), cfg$f_model,
              cfg$from_lang, cfg$to_lang,
              cfg$context, cfg$register, cfg$f_temp,
              prompt_template = cfg$forward_prompt,
              batch_size = cfg$batch_size,
              logger = logger
            )

            batch_rv$progress_pct <- 40
            logger("Forward translation complete")

            cfg$stage <- if (cfg$do_back) "back" else "finalize_sheet"
            batch_rv$state <- cfg
            later(process_batch_step, 0.01)
          },

          back = {
            sheet_status <- if (length(cfg$sheets_to_process) > 1) {
              paste0(" [Sheet: ", cfg$current_sheet, "]")
            } else ""
            update_status(
              batch_rv,
              paste0("\u2190 Back translating batch...", sheet_status),
              "message", session
            )
            logger("Stage: Backward translation")

            batch_rv$progress_pct <- 50
            batch_rv$progress_text <- "Back translating..."

            cfg$df[[cfg$back_col]][cfg$non_empty_idx] <- .pkg$batch_back(
              cfg$df[[cfg$fwd_col]][cfg$non_empty_idx], cfg$b_model,
              cfg$from_lang, cfg$to_lang,
              cfg$context, cfg$register, cfg$b_temp,
              prompt_template = cfg$back_prompt,
              batch_size = cfg$batch_size,
              logger = logger
            )

            batch_rv$progress_pct <- 60
            logger("Backward translation complete")

            cfg$stage <- if (cfg$do_recon) "recon" else "finalize_sheet"
            batch_rv$state <- cfg
            later(process_batch_step, 0.01)
          },

          recon = {
            sheet_status <- if (length(cfg$sheets_to_process) > 1) {
              paste0(" [Sheet: ", cfg$current_sheet, "]")
            } else ""
            update_status(
              batch_rv,
              paste0("\u21bb Reconciling batch...", sheet_status),
              "message", session
            )
            logger("Stage: Reconciliation")

            batch_rv$progress_pct <- 70
            batch_rv$progress_text <- "Reconciling..."

            recon_result <- .pkg$batch_reconcile(
              cfg$items[cfg$non_empty_idx],
              cfg$df[[cfg$fwd_col]][cfg$non_empty_idx],
              cfg$df[[cfg$back_col]][cfg$non_empty_idx],
              cfg$r_model, cfg$from_lang, cfg$to_lang,
              cfg$context, cfg$register, cfg$r_temp,
              prompt_template = cfg$recon_prompt,
              batch_size = cfg$batch_size, logger = logger
            )
            cfg$df[[cfg$recon_col]][cfg$non_empty_idx] <- recon_result$revised
            cfg$df[[cfg$change_col]][cfg$non_empty_idx] <- recon_result$explanation
            cfg$df[[cfg$severity_col]][cfg$non_empty_idx] <- recon_result$severity

            batch_rv$progress_pct <- 90
            logger("Reconciliation complete")

            cfg$stage <- "finalize_sheet"
            batch_rv$state <- cfg
            later(process_batch_step, 0.01)
          },

          finalize_sheet = {
            cfg$all_results[[cfg$current_sheet]] <- cfg$df
            logger(paste("Sheet", cfg$current_sheet, "completed"))

            if (cfg$sheet_idx == 1 && is.null(isolate(batch_rv$preview))) {
              first_idx <- cfg$non_empty_idx[1]
              batch_rv$preview <- list(
                original = cfg$items[first_idx],
                forward = cfg$df[[cfg$fwd_col]][first_idx],
                back = if (!is.null(cfg$back_col)) cfg$df[[cfg$back_col]][first_idx] else NULL,
                reconciled = if (!is.null(cfg$recon_col)) cfg$df[[cfg$recon_col]][first_idx] else NULL
              )
            }

            if (length(cfg$sheets_to_process) > 1) {
              overall_pct <- round((cfg$sheet_idx / length(cfg$sheets_to_process)) * 100)
              batch_rv$progress_pct <- overall_pct
              batch_rv$progress_text <- paste0(
                "Completed ", cfg$sheet_idx, " of ",
                length(cfg$sheets_to_process), " sheets"
              )
            }

            cfg$sheet_idx <- cfg$sheet_idx + 1
            cfg$stage <- "read_sheet"
            batch_rv$state <- cfg
            later(process_batch_step, 0.01)
          },

          done = {
            batch_rv$progress_pct <- 100
            batch_rv$progress_text <- "Complete!"

            if (length(cfg$sheets_to_process) == 1) {
              batch_rv$result <- cfg$all_results[[1]]
              sheet_name <- cfg$sheets_to_process[1]
            } else {
              batch_rv$result <- cfg$all_results
              sheet_name <- NULL
            }

            batch_rv$state <- list(
              from_lang = cfg$from_lang, to_lang = cfg$to_lang,
              forward_prompt = cfg$forward_prompt,
              back_prompt = cfg$back_prompt,
              recon_prompt = cfg$recon_prompt,
              f_model = cfg$f_model, b_model = cfg$b_model, r_model = cfg$r_model,
              f_temp = cfg$f_temp, b_temp = cfg$b_temp, r_temp = cfg$r_temp,
              sheet_name = sheet_name
            )

            batch_rv$running <- FALSE
            if (length(cfg$failed_sheets) > 0) {
              fail_msg <- paste0(
                "Complete with errors on sheet(s): ",
                paste(cfg$failed_sheets, collapse = ", "),
                ". Results may be partial."
              )
              update_status(batch_rv, fail_msg, "warning", session)
              showNotification(fail_msg, type = "warning", duration = NULL, session = session)
              logger(paste("WARNING:", fail_msg))
            } else {
              update_status(batch_rv, "Batch translation complete!", "message", session)
            }
            logger("---- BATCH TRANSLATION END ----")
          }
        )

      }, error = function(e) {
        logger("ERROR:", e$message)
        showNotification(
          paste("Error during batch translation:", e$message),
          type = "error", duration = NULL, session = session
        )
        update_status(batch_rv, paste("Error on sheet:", cfg$current_sheet, "-", e$message), "error", session)
        cfg$failed_sheets <- c(cfg$failed_sheets, cfg$current_sheet)
        if (!is.null(cfg$df)) {
          cfg$all_results[[cfg$current_sheet]] <- cfg$df
          logger(paste("Stored partial results for sheet:", cfg$current_sheet))
        }
        cfg$sheet_idx <- cfg$sheet_idx + 1
        cfg$stage <- "read_sheet"
        batch_rv$state <- cfg
        later(process_batch_step, 0.01)
      })
    }

    later(process_batch_step, 0.01)
  })
}
