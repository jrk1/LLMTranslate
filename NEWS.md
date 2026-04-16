# LLMTranslate 0.4.0

## Breaking Changes

* Replaced hand-rolled API wrappers (`httr2`-based) with [ellmer](https://ellmer.tidyverse.org/) as the LLM communication backbone
* Removed `httr2` dependency; added `ellmer` (>= 0.1.0) to Imports
* API keys are no longer entered in the app UI; set them as environment variables per ellmer's documentation (e.g. `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GOOGLE_API_KEY` in `~/.Renviron`)
* Models are now specified as `provider/model` strings (e.g. `openai/gpt-4.1`, `anthropic/claude-sonnet-4-5-20250929`)
* Removed `MODEL_SPEC`, `NORMALIZE_MAP`, and model alias normalization; the full provider ecosystem is now dynamic
* Replaced `translate()` with two purpose-named exports: `translate_batch()` (all items in one LLM call per stage) and `translate_item()` (one item at a time)

## New Features

* **Provider-agnostic LLM backend**: supports all ellmer providers (OpenAI, Anthropic, Google Gemini, Azure, AWS Bedrock, Ollama, DeepSeek, Groq, Mistral, and more)
* **Dynamic provider dropdown**: the "Model Setup" tab lists all available ellmer providers and auto-populates model lists when the provider's API key is configured
* **Dynamic model discovery**: for providers that support it (OpenAI, Anthropic, Google, Ollama, etc.), available models are fetched and shown in a searchable dropdown; custom model names can also be typed
* **`translate_batch()` / `translate_item()` functions**: New exported functions for programmatic/CLI translation without the Shiny app. Supports all providers, back-translation, and reconciliation with `cli` progress bars
* **Single test button**: "Test Forward Model Connection" verifies that the selected provider/model combination works
* Batch reconciliation now returns a `Recon_Severity` column in both the Shiny app and the programmatic API

## UI Overhaul

* Redesigned the Shiny app as a modern dashboard using `bslib::page_navbar()` with the Flatly theme
* Consolidated file upload, language selection, and column picker into a single shared sidebar on the "Translate" tab — previously duplicated across batch and item-by-item modes
* Translation modes are now toggled via pills within the same page instead of separate tabs
* Model setup uses a card-based layout with three columns (forward, backward, reconciliation)
* Prompts and debug logs are now in collapsible accordions to reduce visual clutter
* "Reset Results" now only clears translation results without affecting file upload or language settings
* Added `bslib` to Imports

## Internal

* Removed ~400 lines of hand-rolled HTTP code (`call_openai_chat()`, `call_openai_reasoning_responses()`, `call_gemini_chat()`, `call_claude_chat()`, `perform_req()`, `create_chat()`, `get_spec()`, `is_reasoning_model()`, `normalize_model()`, `coalesce_chr()`)
* `llm_call()` is now a thin wrapper around `ellmer::chat()`
* `available_providers()` dynamically discovers providers from ellmer namespace
* `available_models()` queries provider-specific model lists via `ellmer::models_*()`
* Extracted batch translation logic into reusable internal helpers (`batch_forward()`, `batch_back()`, `batch_reconcile()`), shared by both exported functions and the Shiny app
* Refactored Shiny batch server to call shared batch helpers instead of inline orchestration
* Fixed `parse_batch_recon_response()` to extract the `severity` field from LLM responses

# LLMTranslate 0.3.0

## Major Features

* **Batch Translation Mode**: New translation mode that sends all items in a single LLM call per stage (forward/back/recon) for faster processing and better context-aware translations
* **Multi-Sheet Excel Support**: Can now select and translate individual sheets or all sheets at once from Excel files with multiple sheets
* **Custom Model Input**: Model selection fields now accept custom model names typed by users, allowing use of newly released models without app updates
* **Empty Row Handling**: Automatically filters and skips empty rows during translation while maintaining correct row alignment
* **Sheet-Specific Status**: Status notifications now indicate which sheet is being processed during multi-sheet translations

## UI/UX Improvements

* Batch Translation tab now appears before Item-by-item Translation as the recommended default
* Added sheet selector dropdown for viewing results from different sheets after multi-sheet translation
* Added "Translation Mode" column to Model Selection Log indicating whether Batch or Item-by-item mode was used
* Added comprehensive Excel file preparation guide in Help tab
* Model selection dropdowns now support typing custom model names with autocomplete
* Custom models trigger informative warnings instead of blocking translation
* Progress text shows current sheet name during multi-sheet processing
* Multi-sheet downloads include all translated sheets plus Model Selection and Prompt logs

## Translation Quality Improvements

* Batch mode prompts emphasize context and terminology consistency across items
* Fixed item number parsing in batch responses to prevent row misalignment
* Parser now uses `item_number` field from LLM responses for accurate mapping
* Improved error handling and logging for batch translation operations

## Bug Fixes

* Fixed preview table error when displaying multi-sheet translation results
* Fixed row mapping issue where translations were offset by one row
* Fixed tryCatch structure in batch translation for proper error handling
* Corrected indentation and control flow in multi-sheet processing loop

# LLMTranslate 0.2.0

## Major Features

* Added support for Anthropic Claude models (Sonnet 4.5, Haiku 4.5, Opus 4.1, and others)
* Updated Google Gemini models to latest versions (Gemini 2.5 Pro, 2.5 Flash, 2.0 Flash)
* Added Model Selection Log sheet to Excel output
* Added Prompt Log sheet to Excel output for full reproducibility

## UI/UX Improvements

* Added visual progress bar showing item X of Y with percentage
* Added "Test Connection" buttons for all API providers with success/error feedback
* Added resume functionality - can continue after stopping mid-batch
* Added warning notification for large batches (500+ items)
* Added real-time preview of first completed translation
* Added "Reset" button to clear all data and start fresh
* Added comprehensive "API Keys Guide" tab with step-by-step instructions for obtaining keys
* Improved Help tab with updated instructions

## Bug Fixes

* Fixed Unicode display issues in API status messages
* Fixed deprecated Gemini model names (updated fallback logic)

# LLMTranslate 0.1.3

# LLMTranslate 0.1.2


# LLMTranslate 0.1.1

* Address CRAN feedback: quotes around software names, expanded acronyms, added references, \value in Rd, minimal test.

# LLMTranslate 0.1.0 (2025-07-24)

* Initial CRAN release.
