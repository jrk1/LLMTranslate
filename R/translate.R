#' @name translate
#' @title Translate survey items using LLMs
#'
#' @description
#' Perform TRAPD/ISPOR-style forward translation with optional
#' back-translation and reconciliation, using any provider supported
#' by ellmer.
#'
#' * `translate_batch()` sends all items in a single LLM call per
#'   stage — faster and better cross-item consistency.
#' * `translate_item()` translates one item at a time — safer for
#'   large surveys, with per-item error isolation.
#'
#' @param data A data frame or path to an Excel file (.xlsx/.xls).
#' @param column Name of the column containing items to translate.
#' @param from_lang Source language (e.g. "English").
#' @param to_lang Target language (e.g. "German").
#' @param model Provider/model string (e.g. "openai/gpt-4.1",
#'   "anthropic/claude-sonnet-4-5-20250929").
#' @param back_model Model for back-translation. Defaults to `model`.
#'   Set to `NULL` to skip back-translation.
#' @param recon_model Model for reconciliation. Defaults to `model`.
#'   Set to `NULL` to skip reconciliation. Ignored if `back_model` is NULL.
#' @param temperature Sampling temperature for LLM calls.
#' @param context Optional instrument context to improve translation quality
#'   (e.g. "This is the PHQ-9 depression screening questionnaire. Items
#'   measure depressive symptoms over the past two weeks.").
#' @param register Optional target population register guidance
#'   (e.g. "Use formal clinical language" or "Use informal language
#'   appropriate for adolescents aged 13-17").
#' @param sheet Sheet name or index for Excel files. Defaults to 1.
#' @param batch_size Maximum number of items per LLM call for
#'   `translate_batch()`. `NULL` (default) sends all items in one
#'   call. Set to e.g. 50 or 100 to split a large questionnaire
#'   into smaller batches — a 400-item survey with `batch_size = 100`
#'   will make 4 calls per stage. Ignored by `translate_item()`.
#' @param verbose Logical; if `TRUE`, prints timestamped debug
#'   messages showing model calls and response previews.
#'
#' @return A data frame with the original data plus added columns:
#'   `Forward_<to_lang>`, and optionally `Back_<from_lang>`,
#'   `Reconciled_<to_lang>`, `Recon_Explanation`, and `Recon_Severity`.
#'   When `verbose = TRUE`, the returned data frame carries an `"log"`
#'   attribute containing the timestamped debug messages (character vector).
#'
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#'   cli_alert_success cli_alert_info cli_alert_danger cli_h2 cli_abort
#' @importFrom glue glue_data
#'
#' @examplesIf interactive()
#' df <- data.frame(item = c("I feel happy", "I feel sad"))
#' translate_batch(df, "item", "English", "German", model = "openai/gpt-4.1")
#' translate_item(df, "item", "English", "German", model = "openai/gpt-4.1")
NULL

# -- shared input prep (not exported) -----------------------------------------

prepare_translate <- function(
  data, column, model, back_model, recon_model,
  context, register, sheet, verbose
) {
  logger <- make_logger(verbose)

  validate_model_string(model, "model")
  if (!is.null(back_model)) validate_model_string(back_model, "back_model")
  if (!is.null(recon_model)) validate_model_string(recon_model, "recon_model")

  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    cli::cli_alert_info("Reading {.file {data}}")
    data <- openxlsx::read.xlsx(data, sheet = sheet)
  }

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame or path to an Excel file.")
  }
  if (!column %in% names(data)) {
    cli::cli_abort("Column {.val {column}} not found in data.")
  }

  do_back <- !is.null(back_model)
  do_recon <- do_back && !is.null(recon_model)

  context_str <- format_context(context)
  register_str <- format_register(register)

  items <- as.character(data[[column]])
  non_empty <- which(!is.na(items) & nzchar(trimws(items)))

  list(
    data = data, items = items, non_empty = non_empty,
    do_back = do_back, do_recon = do_recon,
    context_str = context_str, register_str = register_str,
    logger = logger
  )
}

init_columns <- function(data, from_lang, to_lang, do_back, do_recon) {
  fwd_col <- paste0("Forward_", to_lang)
  back_col <- if (do_back) paste0("Back_", from_lang) else NULL
  recon_col <- if (do_recon) paste0("Reconciled_", to_lang) else NULL
  change_col <- if (do_recon) "Recon_Explanation" else NULL
  severity_col <- if (do_recon) "Recon_Severity" else NULL

  data[[fwd_col]] <- NA_character_
  if (do_back) data[[back_col]] <- NA_character_
  if (do_recon) {
    data[[recon_col]] <- NA_character_
    data[[change_col]] <- NA_character_
    data[[severity_col]] <- NA_character_
  }

  list(
    data = data,
    fwd_col = fwd_col, back_col = back_col,
    recon_col = recon_col, change_col = change_col,
    severity_col = severity_col
  )
}

# -- translate_batch -----------------------------------------------------------

#' @rdname translate
#' @export
translate_batch <- function(
  data,
  column,
  from_lang,
  to_lang,
  model = "openai/gpt-4.1",
  back_model = model,
  recon_model = model,
  temperature = 0,
  context = NULL,
  register = NULL,
  batch_size = NULL,
  sheet = 1,
  verbose = FALSE
) {
  prep <- prepare_translate(
    data, column, model, back_model, recon_model,
    context, register, sheet, verbose
  )
  n <- length(prep$non_empty)
  if (n == 0) {
    cli::cli_alert_info("No non-empty items found in column {.val {column}}.")
    return(prep$data)
  }

  cols <- init_columns(
    prep$data, from_lang, to_lang, prep$do_back, prep$do_recon
  )
  data <- cols$data
  non_empty <- prep$non_empty
  items <- prep$items
  logger <- prep$logger

  cli::cli_h2("Forward translation (batch)")
  cli::cli_alert_info("Using {.val {model}} for {n} item{?s}")
  data[[cols$fwd_col]][non_empty] <- tryCatch(
    batch_forward(
      items[non_empty], model, from_lang, to_lang,
      prep$context_str, prep$register_str, temperature,
      batch_size = batch_size, logger = logger
    ),
    error = function(e) {
      cli::cli_alert_danger("Forward translation failed: {e$message}")
      rep(paste0("ERROR: ", e$message), n)
    }
  )
  cli::cli_alert_success("Forward translation complete ({n} items)")

  if (!prep$do_back) {
    attr(data, "log") <- attr(logger, "get_log")()
    return(data)
  }

  cli::cli_h2("Back-translation (batch)")
  bm <- back_model
  cli::cli_alert_info("Using {.val {bm}} for {n} item{?s}")
  data[[cols$back_col]][non_empty] <- tryCatch(
    batch_back(
      data[[cols$fwd_col]][non_empty], back_model, from_lang, to_lang,
      prep$context_str, prep$register_str, temperature,
      batch_size = batch_size, logger = logger
    ),
    error = function(e) {
      cli::cli_alert_danger("Back-translation failed: {e$message}")
      rep(paste0("ERROR: ", e$message), n)
    }
  )
  cli::cli_alert_success("Back-translation complete ({n} items)")

  if (!prep$do_recon) {
    attr(data, "log") <- attr(logger, "get_log")()
    return(data)
  }

  cli::cli_h2("Reconciliation (batch)")
  rm <- recon_model
  cli::cli_alert_info("Using {.val {rm}} for {n} item{?s}")
  recon_result <- tryCatch(
    batch_reconcile(
      items[non_empty], data[[cols$fwd_col]][non_empty],
      data[[cols$back_col]][non_empty],
      recon_model, from_lang, to_lang,
      prep$context_str, prep$register_str, temperature,
      batch_size = batch_size, logger = logger
    ),
    error = function(e) {
      cli::cli_alert_danger("Reconciliation failed: {e$message}")
      list(
        revised = rep(paste0("ERROR: ", e$message), n),
        explanation = rep("", n),
        severity = rep(NA_character_, n)
      )
    }
  )
  data[[cols$recon_col]][non_empty] <- recon_result$revised
  data[[cols$change_col]][non_empty] <- recon_result$explanation
  data[[cols$severity_col]][non_empty] <- recon_result$severity
  cli::cli_alert_success("Reconciliation complete ({n} items)")

  attr(data, "log") <- attr(logger, "get_log")()
  data
}

# -- translate_item ------------------------------------------------------------

#' @rdname translate
#' @export
translate_item <- function(
  data,
  column,
  from_lang,
  to_lang,
  model = "openai/gpt-4.1",
  back_model = model,
  recon_model = model,
  temperature = 0,
  context = NULL,
  register = NULL,
  sheet = 1,
  verbose = FALSE
) {
  prep <- prepare_translate(
    data, column, model, back_model, recon_model,
    context, register, sheet, verbose
  )
  n <- length(prep$non_empty)
  if (n == 0) {
    cli::cli_alert_info("No non-empty items found in column {.val {column}}.")
    return(prep$data)
  }

  cols <- init_columns(
    prep$data, from_lang, to_lang, prep$do_back, prep$do_recon
  )
  data <- cols$data
  non_empty <- prep$non_empty
  items <- prep$items
  logger <- prep$logger

  cli::cli_h2("Forward translation")
  cli::cli_alert_info("Using {.val {model}}")
  cli::cli_progress_bar("Translating", total = n)
  for (idx in seq_along(non_empty)) {
    i <- non_empty[idx]
    data[[cols$fwd_col]][i] <- tryCatch(
      translate_text(
        items[i], model, from_lang, to_lang,
        prep$context_str, prep$register_str, temperature, prompt_forward,
        logger
      ),
      error = function(e) paste0("ERROR: ", e$message)
    )
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  cli::cli_alert_success("Forward translation complete ({n} items)")

  if (!prep$do_back) {
    attr(data, "log") <- attr(logger, "get_log")()
    return(data)
  }

  cli::cli_h2("Back-translation")
  bm <- back_model
  cli::cli_alert_info("Using {.val {bm}}")
  cli::cli_progress_bar("Back-translating", total = n)
  for (idx in seq_along(non_empty)) {
    i <- non_empty[idx]
    data[[cols$back_col]][i] <- tryCatch(
      translate_text(
        data[[cols$fwd_col]][i], back_model, from_lang, to_lang,
        prep$context_str, prep$register_str, temperature, prompt_back,
        logger
      ),
      error = function(e) paste0("ERROR: ", e$message)
    )
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  cli::cli_alert_success("Back-translation complete ({n} items)")

  if (!prep$do_recon) {
    attr(data, "log") <- attr(logger, "get_log")()
    return(data)
  }

  cli::cli_h2("Reconciliation")
  rm <- recon_model
  cli::cli_alert_info("Using {.val {rm}}")
  cli::cli_progress_bar("Reconciling", total = n)
  for (idx in seq_along(non_empty)) {
    i <- non_empty[idx]
    parsed <- tryCatch(
      reconcile_one(
        items[i], data[[cols$fwd_col]][i], data[[cols$back_col]][i],
        recon_model, from_lang, to_lang,
        prep$context_str, prep$register_str, temperature, prompt_recon,
        logger
      ),
      error = function(e) {
        list(
          revised = paste0("ERROR: ", e$message),
          explanation = "",
          severity = NA_character_
        )
      }
    )
    data[[cols$recon_col]][i] <- parsed$revised
    data[[cols$change_col]][i] <- parsed$explanation
    data[[cols$severity_col]][i] <- parsed$severity %||% NA_character_
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  cli::cli_alert_success("Reconciliation complete ({n} items)")

  attr(data, "log") <- attr(logger, "get_log")()
  data
}
