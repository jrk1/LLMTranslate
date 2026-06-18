## Submission
This is version 0.4.0, a maintenance and reliability update:

* Fixed a crash during translation caused by unresolved prompt placeholders or
  literal curly braces in the source text.
* Fixed GPT-5 / o-series ("reasoning") model calls and large-batch output
  truncation.
* Added automatic retry with backoff for transient API errors (HTTP 429 / 5xx)
  and clearer error messages.
* Custom (user-typed) model names now work for any supported provider, and the
  built-in model list was updated.
* Model selection fields are now freely editable text inputs with a suggestion
  dropdown.

## Test environments
* local Windows 11, R 4.4.2
* win-builder (devel and release)
* R-hub ubuntu-latest, fedora-clang-devel, macos-latest

## R CMD check results
0 errors | 0 warnings | 0 notes

## Notes to CRAN
This package provides a Shiny app. Examples are wrapped in if(interactive()).
All packages used only by the app are in Suggests, as the app is optional; the
package itself depends only on 'shiny' at runtime.
