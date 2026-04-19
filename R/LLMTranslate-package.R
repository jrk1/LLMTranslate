#' LLMTranslate: Shiny App for TRAPD/ISPOR Survey Translation with LLMs
#'
#' A Shiny application that automates forward translation, optional blind
#' back-translation, and reconciliation following the TRAPD/ISPOR workflow.
#' Powered by the ellmer package for provider-agnostic LLM communication.
#'
#' Use [run_app()] to launch the interactive Shiny app, or
#' [translate_batch()] / [translate_item()] for programmatic translation.
#'
#' @seealso [run_app()], [translate_batch()], [translate_item()]
#'
#' @examplesIf interactive()
#' LLMTranslate::run_app()
#'
#' @name LLMTranslate
#' @aliases LLMTranslate-package
#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom rlang %||%
"_PACKAGE"
