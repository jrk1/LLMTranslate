#' Send a prompt to an LLM via ellmer
#'
#' @param model Provider/model string for ellmer::chat(),
#'   e.g. "openai/gpt-4.1" or "anthropic/claude-sonnet-4-5-20250929".
#' @param prompt The prompt text.
#' @param temperature Sampling temperature (NULL to use provider default).
#' @param logger Logger function.
#' @param ... Extra arguments passed to ellmer::chat().
#'
#' @importFrom ellmer chat params
#' @importFrom cli cli_abort
#' @keywords internal
#' @noRd
llm_call <- function(
  model,
  prompt,
  temperature = NULL,
  logger = function(...) {},
  ...
) {
  validate_model_string(model)
  logger("Model:", model, "| Prompt(first 120):", substr(prompt, 1, 120))

  params_args <- list()
  if (!is.null(temperature)) {
    params_args$temperature <- temperature
  }

  args <- list(name = model, echo = "none", ...)
  if (length(params_args)) {
    args$params <- do.call(ellmer::params, params_args)
  }

  chat <- tryCatch(
    do.call(ellmer::chat, args),
    error = function(e) {
      cli::cli_abort(
        "Failed to create chat for model {.val {model}}: {e$message}",
        parent = e
      )
    }
  )
  out <- tryCatch(
    chat$chat(prompt),
    error = function(e) {
      cli::cli_abort(
        "LLM call to {.val {model}} failed: {e$message}",
        parent = e
      )
    }
  )

  logger("Response(first 120):", substr(out, 1, 120))
  out
}
