## Resubmission
This is version 0.4.0, a major refactor replacing hand-rolled API wrappers
with the 'ellmer' package as the LLM communication backbone.

Key changes:
* Replaced 'httr2'-based HTTP code with 'ellmer' for provider-agnostic LLM access
* Added `translate_batch()` and `translate_item()` exported functions for programmatic use
* Redesigned the Shiny app UI using 'bslib'
* Removed 'httr2' dependency; added 'ellmer' (>= 0.1.0) and 'bslib' to Imports

## Test environments
* local macOS (darwin), R 4.4.x
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest (release, devel, oldrel-1)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Notes to CRAN
This package contains a Shiny app. Examples are wrapped in
`\examplesIf{interactive()}`. The app requires API keys for LLM providers,
which are set as environment variables.
