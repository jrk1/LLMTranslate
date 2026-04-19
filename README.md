
# LLMTranslate

<!-- badges: start -->
[![R-CMD-check](https://github.com/jrk1/LLMTranslate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jrk1/LLMTranslate/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**LLMTranslate** automates TRAPD/ISPOR-style survey translation using large language models.
It runs a full three-stage pipeline --- forward translation, blind back-translation, and reconciliation with severity ratings --- in minutes rather than weeks.

Any LLM provider supported by the [ellmer](https://ellmer.tidyverse.org/) package works out of the box: OpenAI, Anthropic, Google Gemini, Azure, Ollama, DeepSeek, Groq, Mistral, and more.

## Key Features

- **Two interfaces**: interactive Shiny app (`run_app()`) and programmatic R functions (`translate_batch()`, `translate_item()`)
- **Batch and item-by-item modes**: batch sends all items in one LLM call for speed and cross-item consistency; item-by-item processes each item separately for large instruments or rate-limited APIs
- **Configurable batch size**: split large instruments into smaller chunks with `batch_size` while keeping batch-mode benefits
- **Multi-sheet Excel support**: translate individual sheets or all sheets at once
- **Provider-agnostic**: specify models as `provider/model` strings (e.g. `"openai/gpt-4.1"`, `"anthropic/claude-sonnet-4-5-20250929"`)
- **Full audit trail**: model selection log, prompt log, and debug log included in downloads; from R, use `translation_log()` to access the debug trace

## Installation

Install from CRAN:

``` r
install.packages("LLMTranslate")
```

Or install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("jrk1/LLMTranslate")
```

## Usage

### Shiny app

``` r
library(LLMTranslate)
run_app()
```

1. Configure your provider and model in the **Setup** tab
2. Upload an Excel file in the **Translate** tab, pick the column with your items
3. Start translation and monitor progress
4. Download results as an Excel workbook with all logs

### From R

``` r
library(LLMTranslate)

df <- data.frame(item = c("I feel happy", "I feel sad"))

result <- translate_batch(
  df, "item",
  from_lang = "English",
  to_lang = "German",
  model = "openai/gpt-4.1"
)
```

Skip back-translation with `back_model = NULL`, or use different models per stage:

``` r
result <- translate_batch(
  df, "item", "English", "German",
  model = "anthropic/claude-sonnet-4-5-20250929",
  back_model = "openai/gpt-4.1-mini",
  recon_model = "anthropic/claude-sonnet-4-5-20250929"
)
```

For large instruments, set `batch_size` to avoid token limits:

``` r
result <- translate_batch(
  df, "item", "English", "German",
  model = "openai/gpt-4.1",
  batch_size = 50
)
```

Access the debug log from a verbose run:

``` r
result <- translate_batch(df, "item", "English", "German",
                          model = "openai/gpt-4.1", verbose = TRUE)
translation_log(result)
```

See `vignette("LLMTranslate")` for the full walkthrough.
