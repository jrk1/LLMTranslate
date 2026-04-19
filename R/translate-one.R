#' Translate a single text using an LLM
#'
#' Shared workhorse for both `translate_item()` and the Shiny app.
#' Applies a glue-style prompt template for LLM calls.
#'
#' **Note on language direction:** `from_lang` and `to_lang` are passed
#' through to the prompt template as-is. For back-translation the caller
#' passes the *same* `from_lang`/`to_lang` as for forward translation;
#' the direction reversal is handled by the prompt template itself
#' (e.g. `prompt_back` asks the LLM to output `{from_lang}` text from
#' `{to_lang}` input).
#'
#' @param text Character scalar to translate.
#' @param model Provider/model string.
#' @param from_lang Source language name.
#' @param to_lang Target language name.
#' @param context_str Pre-formatted context string (empty string if none).
#' @param register_str Pre-formatted register string (empty string if none).
#' @param temperature Sampling temperature.
#' @param prompt_template A glue-style template with placeholders
#'   `{text}`, `{from_lang}`, `{to_lang}`, `{context}`, `{register}`.
#' @param logger Logger function.
#'
#' @return Character scalar with the translated text.
#' @keywords internal
#' @noRd
translate_text <- function(
  text,
  model,
  from_lang,
  to_lang,
  context_str = "",
  register_str = "",
  temperature = 0,
  prompt_template = prompt_forward,
  logger = function(...) {}
) {
  pd <- list(
    text = text,
    from_lang = from_lang,
    to_lang = to_lang,
    context = context_str,
    register = register_str
  )
  prompt <- glue::glue_data(pd, prompt_template)
  llm_call(model, prompt, temperature, logger)
}

#' Reconcile a single item
#'
#' Shared workhorse for the reconciliation stage.
#' Constructs the full prompt from a template, calls the LLM,
#' and parses the JSON response.
#'
#' Note: unlike `translate_text()`, the recon prompt template does
#' **not** use a `{text}` placeholder. Instead, the original, forward,
#' and back texts are appended after the template is resolved. The
#' template should only contain `{from_lang}`, `{to_lang}`, `{context}`,
#' and `{register}` placeholders.
#'
#' @param original Original source text.
#' @param forward Forward translation.
#' @param back Back-translation.
#' @param model Provider/model string.
#' @param from_lang Source language name.
#' @param to_lang Target language name.
#' @param context_str Pre-formatted context string.
#' @param register_str Pre-formatted register string.
#' @param temperature Sampling temperature.
#' @param prompt_template A glue-style template with placeholders
#'   `{from_lang}`, `{to_lang}`, `{context}`, `{register}`.
#'   The original/forward/back texts are appended separately.
#' @param logger Logger function.
#'
#' @return A list with elements `revised`, `explanation`, `severity`.
#' @keywords internal
#' @noRd
reconcile_one <- function(
  original,
  forward,
  back,
  model,
  from_lang,
  to_lang,
  context_str = "",
  register_str = "",
  temperature = 0,
  prompt_template = prompt_recon,
  logger = function(...) {}
) {
  pd <- list(
    from_lang = from_lang,
    to_lang = to_lang,
    context = context_str,
    register = register_str
  )
  r_prompt_body <- glue::glue_data(pd, prompt_template)
  full_prompt <- paste0(
    r_prompt_body,
    "\n---\nORIGINAL:\n", original,
    "\nFORWARD:\n", forward,
    "\nBACK-TRANSLATION:\n", back
  )
  out <- llm_call(model, full_prompt, temperature, logger)
  parse_recon_output(out)
}
