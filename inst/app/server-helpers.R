check_provider_credentials <- function(provider) {
  if (provider == "ollama") {
    return(list(ok = TRUE, msg = "Local server (no API key needed)"))
  }

  ns <- asNamespace("ellmer")
  key_fn <- tryCatch(
    get(paste0(provider, "_key"), envir = ns),
    error = function(e) NULL
  )

  if (!is.null(key_fn) && is.function(key_fn)) {
    has_key <- tryCatch(
      { key_fn(); TRUE },
      error = function(e) FALSE
    )
    if (has_key) {
      return(list(ok = TRUE, msg = "API key configured"))
    }
    return(list(ok = FALSE, msg = "API key not found; see ellmer docs"))
  }

  list(ok = NA, msg = "Check provider docs for auth setup")
}

default_provider <- function(providers) {
  for (p in providers) {
    cred <- check_provider_credentials(p)
    if (isTRUE(cred$ok)) return(p)
  }
  providers[1]
}

render_cred_badge <- function(provider) {
  cred <- check_provider_credentials(provider)
  if (isTRUE(cred$ok)) {
    tags$div(class = "text-success small mb-2", HTML(paste("&#10003;", cred$msg)))
  } else if (isFALSE(cred$ok)) {
    tags$div(class = "text-danger small mb-2", HTML(paste("&#10007;", cred$msg)))
  } else {
    tags$div(class = "text-muted small mb-2", HTML(paste("&#9432;", cred$msg)))
  }
}

make_file_rv <- function() {
  reactiveValues(
    df = NULL, file_reset = 0, file_path = NULL,
    selected_sheet = NULL, sheets = NULL
  )
}

make_mode_rv <- function() {
  reactiveValues(
    result = NULL, log = character(),
    running = FALSE, stop = FALSE,
    state = NULL, notif_id = NULL,
    preview = NULL, progress_pct = 0, progress_text = ""
  )
}

make_model_ui <- function(provider, input_id) {
  models <- .pkg$available_models(provider)
  if (is.null(models)) {
    selectizeInput(
      input_id, "Model",
      choices = NULL, selected = NULL,
      options = list(create = TRUE, placeholder = "Type a model name")
    )
  } else {
    selectizeInput(
      input_id, "Model",
      choices = models, selected = models[1],
      options = list(create = TRUE, placeholder = "Select or type a model name")
    )
  }
}

make_lang_ui <- function(input_id, label, default) {
  textInput(input_id, label, value = default)
}

full_model <- function(input, provider_input, model_input) {
  provider <- input[[provider_input]]
  if (is.null(provider)) return("")
  model <- input[[model_input]]
  if (is.null(model) || !nzchar(model)) return("")
  paste0(provider, "/", model)
}

update_status <- function(mode_rv, msg, type = "message", session = getDefaultReactiveDomain()) {
  old_id <- isolate(mode_rv$notif_id)
  if (!is.null(old_id)) {
    try(removeNotification(old_id, session = session), silent = TRUE)
  }
  id <- showNotification(
    msg, type = type, duration = NULL,
    closeButton = FALSE, session = session
  )
  isolate(mode_rv$notif_id <- id)
}

make_app_logger <- function(mode_rv, enabled) {
  .pkg$make_logger(enabled, sink = mode_rv, max_lines = MAX_LOG_LINES)
}

read_file_safely <- function(path) {
  tryCatch(
    openxlsx::getSheetNames(path),
    error = function(e) {
      showNotification(
        paste("Could not read Excel file:", e$message),
        type = "error", duration = 8
      )
      NULL
    }
  )
}

build_col_selector <- function(
  input, file_rv, file_input_id,
  sheet_input_id, col_input_id
) {
  file_input <- input[[file_input_id]]
  req(file_input)

  sheets <- file_rv$sheets
  if (is.null(sheets)) {
    sheets <- read_file_safely(file_input$datapath)
    if (is.null(sheets)) return(NULL)
  }

  sheet_choices <- if (length(sheets) > 1) c("All sheets", sheets) else sheets
  selected_sheet <- if (!is.null(input[[sheet_input_id]])) {
    input[[sheet_input_id]]
  } else if (length(sheets) > 1) {
    "All sheets"
  } else {
    sheets[1]
  }

  preview_sheet <- if (identical(selected_sheet, "All sheets")) sheets[1] else selected_sheet

  df_head <- tryCatch(
    openxlsx::read.xlsx(file_input$datapath, sheet = preview_sheet, rows = 1:2),
    error = function(e) {
      showNotification(
        paste("Error reading sheet:", e$message),
        type = "error", duration = 8
      )
      NULL
    }
  )
  if (is.null(df_head)) return(NULL)

  tagList(
    if (length(sheets) > 1) {
      selectInput(
        sheet_input_id, "Select sheet(s)",
        choices = sheet_choices, selected = selected_sheet
      )
    },
    selectInput(col_input_id, "Column with ORIGINAL item", choices = names(df_head))
  )
}

load_file_data <- function(input, file_rv, file_input_id, sheet_input_id) {
  file_input <- input[[file_input_id]]
  if (is.null(file_input)) return()

  sheets <- file_rv$sheets
  if (is.null(sheets)) {
    sheets <- read_file_safely(file_input$datapath)
    if (is.null(sheets)) return()
    file_rv$sheets <- sheets
  }

  selected_sheet <- if (!is.null(input[[sheet_input_id]])) {
    input[[sheet_input_id]]
  } else if (length(sheets) > 1) {
    "All sheets"
  } else {
    sheets[1]
  }
  preview_sheet <- if (identical(selected_sheet, "All sheets")) sheets[1] else selected_sheet

  tryCatch({
    file_rv$df <- openxlsx::read.xlsx(file_input$datapath, sheet = preview_sheet)
    file_rv$selected_sheet <- selected_sheet
    file_rv$file_path <- file_input$datapath
  }, error = function(e) {
    showNotification(
      paste("Error reading file:", e$message),
      type = "error", duration = 8
    )
  })
}

reset_mode_rv <- function(session, mode_rv) {
  mode_rv$result <- NULL
  mode_rv$log <- character()
  mode_rv$running <- FALSE
  mode_rv$stop <- FALSE
  mode_rv$state <- NULL
  mode_rv$notif_id <- NULL
  mode_rv$preview <- NULL
  mode_rv$progress_pct <- 0
  mode_rv$progress_text <- ""
  showNotification("Results cleared.", type = "message")
}

bind_prompt_preview <- function(output, input, output_id, template) {
  output[[output_id]] <- renderUI({
    tags$pre(
      class = "bg-light p-2 small", style = "white-space:pre-wrap;",
      resolve_prompt_preview(
        template, input$lang_from, input$lang_to,
        input$instrument_context, input$target_register
      )
    )
  })
}

resolve_prompt_preview <- function(template, from_lang, to_lang, context, register) {
  ctx <- .pkg$format_context(context)
  reg <- .pkg$format_register(register)
  as.character(glue::glue_data(
    list(
      from_lang = from_lang, to_lang = to_lang,
      context = ctx, register = reg,
      text = "<your item text>",
      items_text = "<your survey items>"
    ),
    template
  ))
}

build_download_content <- function(
  input, mode_rv, mode_label,
  default_fwd, default_back, default_recon
) {
  cfg <- mode_rv$state

  model_log <- data.frame(
    `Translation Mode` = mode_label,
    Step = c("Forward Translation", "Backward Translation", "Reconciliation"),
    Model = c(
      if (!is.null(cfg)) cfg$f_model else full_model(input, "forward_provider", "forward_model"),
      if (!is.null(cfg)) cfg$b_model else full_model(input, "back_provider", "back_model"),
      if (!is.null(cfg)) cfg$r_model else full_model(input, "recon_provider", "recon_model")
    ),
    Temperature = c(
      if (!is.null(cfg)) cfg$f_temp else input$forward_temp,
      if (!is.null(cfg)) cfg$b_temp else input$back_temp,
      if (!is.null(cfg)) cfg$r_temp else input$recon_temp
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  prompt_log <- data.frame(
    Step = c("Forward Translation", "Backward Translation", "Reconciliation"),
    Prompt = c(
      if (!is.null(cfg)) cfg$forward_prompt else default_fwd,
      if (!is.null(cfg)) cfg$back_prompt else default_back,
      if (!is.null(cfg)) cfg$recon_prompt else default_recon
    ),
    stringsAsFactors = FALSE
  )

  list(model_log = model_log, prompt_log = prompt_log)
}

render_progress_ui <- function(mode_rv) {
  req(mode_rv$running, mode_rv$progress_text)
  div(
    class = "progress-container",
    div(
      class = "progress",
      div(
        class = "progress-bar progress-bar-striped progress-bar-animated",
        role = "progressbar",
        style = paste0("width: ", mode_rv$progress_pct, "%"),
        mode_rv$progress_text
      )
    )
  )
}

render_preview_ui <- function(
  mode_rv,
  first_item_label = "First Translation Preview"
) {
  req(mode_rv$preview)
  div(
    class = "preview-box",
    h5(HTML(paste0("&#10003; ", first_item_label, " (verify quality and stop if needed)"))),
    div(class = "preview-item",
        span(class = "preview-label", "Original: "), mode_rv$preview$original),
    div(class = "preview-item",
        span(class = "preview-label", "Forward: "), mode_rv$preview$forward),
    if (!is.null(mode_rv$preview$back))
      div(class = "preview-item",
          span(class = "preview-label", "Back: "), mode_rv$preview$back),
    if (!is.null(mode_rv$preview$reconciled))
      div(class = "preview-item",
          span(class = "preview-label", "Reconciled: "), mode_rv$preview$reconciled)
  )
}

validate_before_run <- function(file_rv, input, orig_col_id) {
  if (is.null(file_rv$df)) {
    showNotification("Please upload an Excel file first.", type = "error", duration = 5)
    return(FALSE)
  }
  col <- input[[orig_col_id]]
  if (is.null(col) || !nzchar(col)) {
    showNotification("Please select a column with original items.", type = "error", duration = 5)
    return(FALSE)
  }
  if (!col %in% names(file_rv$df)) {
    showNotification(
      paste0("Column '", col, "' not found in the uploaded data. Please re-select."),
      type = "error", duration = 5
    )
    return(FALSE)
  }

  f_model <- full_model(input, "forward_provider", "forward_model")
  valid <- tryCatch(
    { .pkg$validate_model_string(f_model, "forward model"); TRUE },
    error = function(e) {
      showNotification(
        "Please select a forward model in the Setup tab.",
        type = "error", duration = 5
      )
      FALSE
    }
  )
  if (!valid) return(FALSE)

  TRUE
}
