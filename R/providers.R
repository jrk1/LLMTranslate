#' List providers supported by ellmer::chat()
#'
#' Discovers providers by introspecting ellmer's namespace for
#' `chat_*()` functions whose formals include `model`, `system_prompt`,
#' and `params`. Non-provider utilities (e.g. `chat_browser`) are
#' excluded via a known blocklist. If ellmer changes its function
#' signatures this heuristic may need updating.
#'
#' @keywords internal
#' @noRd
available_providers <- function() {
  ns <- asNamespace("ellmer")
  chat_fns <- ls(ns, pattern = "^chat_")
  required_args <- c("model", "system_prompt", "params")

  is_provider <- vapply(
    chat_fns,
    function(fn_name) {
      fn <- get(fn_name, envir = ns)
      is.function(fn) && all(required_args %in% names(formals(fn)))
    },
    logical(1)
  )

  providers <- sub("^chat_", "", chat_fns[is_provider])
  exclude <- c("_test$", "^browser$", "^console$")
  keep <- !Reduce(`|`, lapply(exclude, grepl, x = providers))
  providers[keep]
}

#' Try to fetch available models for a provider
#'
#' Calls the ellmer models_* function if one exists and the
#' provider's API key is configured. Returns a character vector
#' of model IDs, or NULL on failure.
#'
#' @param provider Character scalar; a provider name
#'   (e.g. `"openai"`, `"anthropic"`).
#' @keywords internal
#' @noRd
available_models <- function(provider) {
  fn_name <- paste0("models_", provider)
  ns <- asNamespace("ellmer")
  fn <- tryCatch(get(fn_name, envir = ns), error = function(e) NULL)
  if (is.null(fn) || !is.function(fn)) {
    return(NULL)
  }
  tryCatch(
    {
      result <- fn()
      if (is.data.frame(result) && "id" %in% names(result)) {
        result$id
      } else if (is.character(result)) {
        result
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )
}
