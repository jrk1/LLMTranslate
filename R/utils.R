#' Validate a model string (e.g. "openai/gpt-4.1")
#'
#' @param model Value to validate.
#' @param arg Argument name for error messages.
#' @keywords internal
#' @noRd
validate_model_string <- function(model, arg = "model") {
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    cli::cli_abort(
      "{.arg {arg}} must be a non-empty string (e.g. {.val openai/gpt-4.1})."
    )
  }
  if (!grepl("/", model, fixed = TRUE)) {
    cli::cli_abort(
      "{.arg {arg}} must use {.val provider/model} format (e.g. {.val openai/gpt-4.1}), got {.val {model}}."
    )
  }
  invisible(model)
}

#' Format context and register strings for prompt insertion
#'
#' @param context,register Scalar character or NULL.
#' @name format_helpers
#' @keywords internal
#' @noRd
format_context <- function(context) {
  if (!is.null(context) && length(context) == 1L && nzchar(context)) {
    paste0("\n\nInstrument context: ", context)
  } else {
    ""
  }
}

#' @rdname format_helpers
#' @noRd
format_register <- function(register) {
  if (!is.null(register) && length(register) == 1L && nzchar(register)) {
    paste0("\n\nTarget register: ", register)
  } else {
    ""
  }
}

#' Create a timestamped logger function
#'
#' @param enabled Logical; if `FALSE`, the logger is a no-op.
#' @param sink Optional `reactiveValues` object with a `log` element
#'   (used by the Shiny app).
#' @param max_lines Maximum log lines to retain in `sink`.
#' @keywords internal
#' @noRd
make_logger <- function(enabled, sink = NULL, max_lines = 500L) {
  force(enabled)
  force(sink)
  function(...) {
    if (!enabled) return(invisible())
    msg <- paste0(
      format(Sys.time(), "%H:%M:%S"), " | ",
      paste(..., collapse = " ")
    )
    cli::cli_inform(msg)
    if (!is.null(sink)) {
      shiny::isolate({
        log <- sink$log
        if (length(log) >= max_lines) {
          n_keep <- max_lines %/% 2L
          n_head <- n_keep %/% 4L
          n_tail <- n_keep - n_head
          log <- c(
            log[seq_len(n_head)],
            "--- [log trimmed] ---",
            log[seq.int(length(log) - n_tail + 1L, length(log))]
          )
        }
        sink$log <- c(log, msg)
      })
    }
    invisible()
  }
}
