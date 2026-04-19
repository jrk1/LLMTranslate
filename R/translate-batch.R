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
  batch_size = NULL,
  logger = function(...) {}
) {
  items <- as.character(items)
  chunks <- chunk_seq(length(items), batch_size)

  if (length(chunks) > 1L) {
    logger(sprintf(
      "Splitting %d items into %d batches of up to %d",
      length(items), length(chunks), batch_size
    ))
  }

  results <- character(length(items))

  for (ci in seq_along(chunks)) {
    idx <- chunks[[ci]]
    chunk_items <- items[idx]

    if (length(chunks) > 1L) {
      logger(sprintf("Batch %d/%d (%d items)", ci, length(chunks), length(idx)))
    }

    items_text <- paste(
      paste0(seq_along(chunk_items), ". ", chunk_items),
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
    parsed <- parse_batch_response(response, field_name, length(chunk_items), logger)
    results[idx] <- vapply(parsed, function(x) {
      val <- x %||% NA_character_
      if (is.list(val) || length(val) != 1L) return(NA_character_)
      as.character(val)
    }, character(1))
  }

  results
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
  batch_size = NULL,
  logger = function(...) {}
) {
  batch_translate(
    items, model, from_lang, to_lang,
    field_name = "translation",
    context_str, register_str, temperature, prompt_template,
    batch_size, logger
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
  batch_size = NULL,
  logger = function(...) {}
) {
  batch_translate(
    items, model, from_lang, to_lang,
    field_name = "back_translation",
    context_str, register_str, temperature, prompt_template,
    batch_size, logger
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
  batch_size = NULL,
  logger = function(...) {}
) {
  originals <- as.character(originals)
  forwards <- as.character(forwards)
  backs <- as.character(backs)
  stopifnot(
    length(originals) == length(forwards),
    length(forwards) == length(backs)
  )

  chunks <- chunk_seq(length(originals), batch_size)

  if (length(chunks) > 1L) {
    logger(sprintf(
      "Splitting %d items into %d reconciliation batches of up to %d",
      length(originals), length(chunks), batch_size
    ))
  }

  revised <- character(length(originals))
  explanation <- character(length(originals))
  severity <- character(length(originals))

  for (ci in seq_along(chunks)) {
    idx <- chunks[[ci]]

    if (length(chunks) > 1L) {
      logger(sprintf("Reconciliation batch %d/%d (%d items)", ci, length(chunks), length(idx)))
    }

    chunk_orig <- originals[idx]
    chunk_fwd <- forwards[idx]
    chunk_back <- backs[idx]

    item_nums <- seq_along(chunk_orig)
    items_text <- paste(
      paste0(
        "Item ", item_nums, ":\n",
        "ORIGINAL: ", chunk_orig, "\n",
        "FORWARD: ", chunk_fwd, "\n",
        "BACK: ", chunk_back, "\n"
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

    parsed <- parse_batch_recon_response(response, length(chunk_orig), logger)

    revised[idx] <- vapply(parsed, `[[`, "", "revised")
    explanation[idx] <- vapply(parsed, `[[`, "", "explanation")
    severity[idx] <- vapply(
      parsed,
      function(x) as.character(x[["severity"]] %||% NA_character_),
      character(1)
    )
  }

  list(revised = revised, explanation = explanation, severity = severity)
}
