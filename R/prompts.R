#' Default prompt templates for TRAPD/ISPOR translation
#'
#' All prompts use glue-style `{variable}` placeholders.
#' Optional fields (`{context}`, `{register}`) are empty strings
#' by default and are only included when the user provides values.
#' Literal braces in JSON examples are escaped as `{{` / `}}` per
#' glue conventions.
#'
#' @name prompts
#' @noRd
NULL

#' @rdname prompts
#' @noRd
prompt_forward <- paste(
  "You are a professional survey translator following TRAPD/ISPOR best practices.",
  "Your goal is conceptual equivalence: the {to_lang} version must measure the same",
  "construct as the original, not be a word-for-word literal translation.",
  "",
  "Constraints:",
  "- Preserve meaning, intent, item polarity (positive/negative wording), numbers,",
  "  quantifiers, modality, and time references.",
  "- Preserve response scale anchors exactly if present (e.g. Strongly agree ... Strongly disagree).",
  "- Avoid culture-bound idioms or metaphors unless an equivalent exists in {to_lang}.",

  "- When a direct translation is natural and equivalent, prefer it over a paraphrase.",
  "- Keep reading level, register, and tone comparable to the source.{register}",
  "{context}",
  "",
  "Output ONLY the final {to_lang} text. No quotes, labels, or explanations.",
  "",
  "ITEM ({from_lang}):",
  "{text}",
  sep = "\n"
)

#' @rdname prompts
#' @noRd
prompt_back <- paste(
  "You are performing a blind back-translation for a TRAPD/ISPOR quality check.",
  "You have NOT seen the original {from_lang} item. You are working only from",
  "the {to_lang} text below.",
  "",
  "Rules:",
  "- Translate as literally as possible while remaining grammatical in {from_lang}.",
  "- Do NOT improve, clarify, or embellish the wording. Reflect exactly what the",
  "  {to_lang} text says, including any awkwardness.",
  "- Preserve polarity, quantifiers, modality, tense, and response scale anchors.",
  "- No comments, brackets, or explanations.",
  "",
  "Output ONLY the {from_lang} text.",
  "",
  "ITEM ({to_lang}):",
  "{text}",
  sep = "\n"
)

#' @rdname prompts
#' @noRd
prompt_recon <- paste(
  "You are reconciling a survey translation (TRAPD/ISPOR adjudication step).",
  "",
  "Inputs:",
  "1) ORIGINAL ({from_lang})",
  "2) FORWARD translation ({to_lang})",
  "3) BACK-TRANSLATION ({from_lang}) - a blind literal re-translation of FORWARD",
  "",
  "Tasks:",
  "1. Compare ORIGINAL and BACK-TRANSLATION to identify meaning shifts, omissions,",
  "   additions, or changes in polarity, intensity, or scope.",
  "2. Classify the severity:",
  "   - \"none\": FORWARD is conceptually equivalent. No change needed.",
  "   - \"minor\": Small stylistic difference that does not affect construct measurement.",
  "   - \"major\": Meaning shift, polarity change, omission, or addition that could",
  "     affect how respondents interpret or answer the item.",
  "3. If severity is \"major\", revise FORWARD to restore equivalence with ORIGINAL",
  "   while remaining natural in {to_lang}. Keep tone, register, and length similar.",
  "   If severity is \"none\" or \"minor\", return FORWARD unchanged.{register}",
  "{context}",
  "",
  "Return a JSON object with exactly these keys:",
  "{{",
  "  \"revised\": \"<the {to_lang} item text (revised if needed, otherwise unchanged)>\",",
  "  \"severity\": \"none|minor|major\",",
  "  \"explanation\": \"<what differs between ORIGINAL and BACK, and why revision was or was not needed>\"",
  "}}",
  "No other text outside the JSON.",
  sep = "\n"
)

# ---- Batch prompts ----

#' @rdname prompts
#' @noRd
prompt_batch_forward <- paste(
  "You are a professional survey translator following TRAPD/ISPOR best practices.",
  "Goal: translate ALL items from {from_lang} to {to_lang}.",
  "",
  "You are translating an entire survey instrument. Use the full context of all items",
  "to ensure terminological consistency across the instrument.",
  "",
  "Constraints:",
  "- Aim for conceptual equivalence, not word-for-word literal translation.",
  "- Preserve meaning, intent, item polarity (positive/negative wording), numbers,",
  "  quantifiers, modality, time references, and response scale anchors.",
  "- Maintain consistent terminology for recurring concepts across all items.",
  "- Preserve reverse-coded item directionality.",
  "- Avoid culture-bound idioms or metaphors unless an equivalent exists in {to_lang}.",
  "- Keep reading level, register, and tone comparable to the source.{register}",
  "{context}",
  "",
  "Output format:",
  "- Return a JSON array with one object per item.",
  "- Each object: {{\"item_number\": N, \"translation\": \"translated text\"}}",
  "- Preserve the item order. Output ONLY the JSON array.",
  "",
  "ITEMS ({from_lang}):",
  "{items_text}",
  sep = "\n"
)

#' @rdname prompts
#' @noRd
prompt_batch_back <- paste(
  "You are performing blind back-translation for a TRAPD/ISPOR quality check.",
  "You have NOT seen the original {from_lang} items. Work only from the {to_lang}",
  "texts below.",
  "",
  "Rules:",
  "- Translate as literally as possible while remaining grammatical in {from_lang}.",
  "- Do NOT improve, clarify, or embellish. Reflect exactly what each {to_lang} item says.",
  "- Preserve polarity, quantifiers, modality, tense, and response scale anchors.",
  "- Maintain consistency across items.",
  "",
  "Output format:",
  "- Return a JSON array with one object per item.",
  "- Each object: {{\"item_number\": N, \"back_translation\": \"back-translated text\"}}",
  "- Preserve the item order. Output ONLY the JSON array.",
  "",
  "ITEMS ({to_lang}):",
  "{items_text}",
  sep = "\n"
)

#' @rdname prompts
#' @noRd
prompt_batch_recon <- paste(
  "You are reconciling survey translations (TRAPD/ISPOR adjudication step).",
  "",
  "For each item you receive:",
  "1) ORIGINAL ({from_lang})",
  "2) FORWARD ({to_lang})",
  "3) BACK-TRANSLATION ({from_lang}) - a blind literal re-translation of FORWARD",
  "",
  "For each item:",
  "1. Compare ORIGINAL and BACK-TRANSLATION to identify meaning shifts, omissions,",
  "   additions, or changes in polarity, intensity, or scope.",
  "2. Classify severity: \"none\", \"minor\", or \"major\" (see definitions below).",
  "3. If \"major\", revise FORWARD to restore equivalence. Otherwise return FORWARD unchanged.",
  "- Maintain terminological consistency across all items.{register}",
  "{context}",
  "",
  "Severity definitions:",
  "- \"none\": Conceptually equivalent. No change needed.",
  "- \"minor\": Small stylistic difference. Does not affect construct measurement.",
  "- \"major\": Meaning shift, polarity change, omission, or addition that could",
  "  affect respondent interpretation.",
  "",
  "Output format:",
  "- Return a JSON array with one object per item.",
  "- Each object: {{\"item_number\": N, \"revised\": \"revised {to_lang} text\",",
  "  \"severity\": \"none|minor|major\",",
  "  \"explanation\": \"what differs and why revision was or was not needed\"}}",
  "- Preserve item order. Output ONLY the JSON array.",
  "",
  "ITEMS:",
  "{items_text}",
  sep = "\n"
)
