library(LLMTranslate)
library(shiny)
library(bslib)
library(DT)

.pkg <- asNamespace("LLMTranslate")

ALL_PROVIDERS <- .pkg$available_providers()

DEFAULT_FORWARD       <- .pkg$prompt_forward
DEFAULT_BACK          <- .pkg$prompt_back
DEFAULT_RECON_UI      <- .pkg$prompt_recon
DEFAULT_BATCH_FORWARD <- .pkg$prompt_batch_forward
DEFAULT_BATCH_BACK    <- .pkg$prompt_batch_back
DEFAULT_BATCH_RECON   <- .pkg$prompt_batch_recon

MAX_LOG_LINES <- 500L
