
# LLMTranslate

<!-- badges: start -->
<!-- badges: end -->

**LLMTranslate** is an R package that wraps a Shiny application for TRAPD/ISPOR-style survey translation. It automates forward translation, optional back-translation, and reconciliation using large language models from multiple providers:

- **OpenAI**: GPT-4o, GPT-4.1, GPT-5 series, and o-series reasoning models
- **Google Gemini**: Gemini 2.5 Pro, 2.5 Flash, 2.0 Flash
- **Anthropic Claude**: Claude Sonnet 4.5, Claude Haiku 4.5, Claude Opus 4.1, and more


## Installation

You can install the development version of LLMTranslate like so:

``` r
install.packages("LLMTranslate")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LLMTranslate)
run_app()
```

