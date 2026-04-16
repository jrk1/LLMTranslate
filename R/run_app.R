# nocov start

#' Launch the LLM Survey Translator shiny app
#'
#' @param ... Passed to shiny::runApp.
#' @return No return value, called for side effects (launches a shiny app).
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom glue glue_data
#' @importFrom later later
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook read.xlsx getSheetNames
#' @export
#'
#' @examplesIf interactive()
#' run_app()
run_app <- function(...) {
  app_dir <- system.file("app", package = "LLMTranslate")
  shiny::runApp(app_dir, ...)
}
# nocov end
