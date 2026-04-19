#' Extract results or log from a translation
#'
#' Convenience accessors for objects returned by [translate_batch()]
#' or [translate_item()].
#'
#' * `translation_log()` returns the timestamped debug messages
#'   (character vector).  Returns `character(0)` when the translation
#'   was run with `verbose = FALSE`.
#' * `translation_result()` returns the data frame stripped of the
#'   `"log"` attribute, so it can be passed to downstream code that
#'   does not expect extra attributes.
#'
#' @param x A data frame returned by [translate_batch()] or
#'   [translate_item()].
#' @return `translation_log()`: character vector of log lines.
#'
#'   `translation_result()`: the same data frame without the `"log"`
#'   attribute.
#' @name translation_accessors
#'
#' @examplesIf interactive()
#' result <- translate_batch(
#'   data.frame(item = c("I feel happy", "I feel sad")),
#'   "item", "English", "German",
#'   model = "openai/gpt-4.1", verbose = TRUE
#' )
#' translation_log(result)
#' translation_result(result)
NULL

#' @rdname translation_accessors
#' @export
translation_log <- function(x) {
  log <- attr(x, "log")
  if (is.null(log)) character(0) else log
}

#' @rdname translation_accessors
#' @export
translation_result <- function(x) {
  attr(x, "log") <- NULL
  x
}
