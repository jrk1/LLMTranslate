#' Translate a batch of items in a single LLM call
#'
#' Shared workhorse for forward and back batch translation.
#'
#' @param items Character vector of items to translate.
#' @param model Provider/model string.
#' @param from_lang Source language name.
#' @param to_lang Target language name.
#' @param field_name JSON field to extract from response
#'   (e.g. `"translation"` or `"back_translation"`).
#' @param context_str Pre-formatted context string.
#' @param register_str Pre-formatted register string.
#' @param temperature Sampling temperature.
#' @param prompt_template Glue-style template.
#' @param logger Logger function.
#'
#' @return Character vector the same length as `items`.
#' @keywords internal
#' @noRd
batch_translate <- function(
  items,
  model,
  from_lang,
  to_lang,
  field_name,
  context_str = "",
  register_str = "",
  temperature = 0,
  prompt_template,
  logger = function(...) {}
) {
  items <- as.character(items)
  items_text <- paste(
    paste0(seq_along(items), ". ", items),
    collapse = "\n"
  )
  pd <- list(
    items_text = items_text,
    from_lang = from_lang,
    to_lang = to_lang,
    context = context_str,
    register = register_str
  )
  prompt <- glue::glue_data(pd, prompt_template)
  response <- llm_call(model, prompt, temperature, logger)
  logger("Batch response received, parsing...")
  parsed <- parse_batch_response(response, field_name, length(items), logger)
  vapply(parsed, function(x) {
    val <- x %||% NA_character_
    if (is.list(val) || length(val) != 1L) return(NA_character_)
    as.character(val)
  }, character(1))
}

#' Forward-translate a batch of items in a single LLM call
#'
#' @param items Character vector of items to translate.
#' @param model Provider/model string.
#' @param from_lang Source language name.
#' @param to_lang Target language name.
#' @param context_str Pre-formatted context string.
#' @param register_str Pre-formatted register string.
#' @param temperature Sampling temperature.
#' @param prompt_template Glue-style template; defaults to
#'   `prompt_batch_forward`.
#' @param logger Logger function.
#'
#' @return Character vector the same length as `items`.
#' @keywords internal
#' @noRd
batch_forward <- function(
  items,
  model,
  from_lang,
  to_lang,
  context_str = "",
  register_str = "",
  temperature = 0,
  prompt_template = prompt_batch_forward,
  logger = function(...) {}
) {
  batch_translate(
    items, model, from_lang, to_lang,
    field_name = "translation",
    context_str, register_str, temperature, prompt_template, logger
  )
}

#' Back-translate a batch of items in a single LLM call
#'
#' **Note on language direction:** `from_lang` and `to_lang` are passed
#' through to the prompt template as-is (same values as for forward
#' translation). The direction reversal is handled by the prompt
#' template itself — `prompt_batch_back` asks the LLM to output
#' `{from_lang}` text from `{to_lang}` input.
#'
#' @inheritParams batch_forward
#' @param prompt_template Glue-style template; defaults to
#'   `prompt_batch_back`.
#'
#' @return Character vector the same length as `items`.
#' @keywords internal
#' @noRd
batch_back <- function(
  items,
  model,
  from_lang,
  to_lang,
  context_str = "",
  register_str = "",
  temperature = 0,
  prompt_template = prompt_batch_back,
  logger = function(...) {}
) {
  batch_translate(
    items, model, from_lang, to_lang,
    field_name = "back_translation",
    context_str, register_str, temperature, prompt_template, logger
  )
}

#' Reconcile a batch of items in a single LLM call
#'
#' @param originals Character vector of original source items.
#' @param forwards Character vector of forward translations.
#' @param backs Character vector of back-translations.
#' @param model Provider/model string.
#' @param from_lang Source language name.
#' @param to_lang Target language name.
#' @param context_str Pre-formatted context string.
#' @param register_str Pre-formatted register string.
#' @param temperature Sampling temperature.
#' @param prompt_template Glue-style template; defaults to
#'   `prompt_batch_recon`.
#' @param logger Logger function.
#'
#' @return A named list with character vectors `revised`,
#'   `explanation`, and `severity`, each the same length as
#'   `originals`.
#' @keywords internal
#' @noRd
batch_reconcile <- function(
  originals,
  forwards,
  backs,
  model,
  from_lang,
  to_lang,
  context_str = "",
  register_str = "",
  temperature = 0,
  prompt_template = prompt_batch_recon,
  logger = function(...) {}
) {
  originals <- as.character(originals)
  forwards <- as.character(forwards)
  backs <- as.character(backs)
  stopifnot(
    length(originals) == length(forwards),
    length(forwards) == length(backs)
  )
  item_nums <- seq_along(originals)
  items_text <- paste(
    paste0(
      "Item ", item_nums, ":\n",
      "ORIGINAL: ", originals, "\n",
      "FORWARD: ", forwards, "\n",
      "BACK: ", backs, "\n"
    ),
    collapse = "\n"
  )

  pd <- list(
    items_text = items_text,
    from_lang = from_lang,
    to_lang = to_lang,
    context = context_str,
    register = register_str
  )
  prompt <- glue::glue_data(pd, prompt_template)
  response <- llm_call(model, prompt, temperature, logger)
  logger("Reconciliation batch response received, parsing...")

  parsed <- parse_batch_recon_response(response, length(originals), logger)

  list(
    revised = vapply(parsed, `[[`, "", "revised"),
    explanation = vapply(parsed, `[[`, "", "explanation"),
    severity = vapply(
      parsed,
      function(x) as.character(x[["severity"]] %||% NA_character_),
      character(1)
    )
  )
}
