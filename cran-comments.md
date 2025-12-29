## Resubmission
This is a new version 0.3.0 with major new features:
* Added batch translation mode for faster, context-aware translations
* Added multi-sheet Excel file support
* Added custom model name input capability
* Improved UI/UX and documentation

## Test environments
* local Windows 11, R 4.4.0
* win-builder (devel and release)
* R-hub ubuntu-latest, fedora-clang-devel, macos-latest

## R CMD check results
0 errors | 0 warnings | 0 notes

## Notes to CRAN
This package contains a Shiny app. Examples are wrapped in if(interactive()).
All package dependencies are in Suggests as the app is optional.
