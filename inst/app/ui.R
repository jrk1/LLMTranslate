mode_panel_ui <- function(prefix, run_label, is_batch = FALSE) {
  tagList(
    div(
      class = "d-flex gap-2 align-items-center mb-3 mt-3",
      actionButton(paste0(prefix, "_run"), run_label, class = "btn-primary"),
      actionButton(paste0(prefix, "_stop"), "Stop", class = "btn-danger"),
      actionButton(paste0(prefix, "_reset"), "Reset Results", class = "btn-warning"),
      uiOutput(paste0(prefix, "_download_ui"), inline = TRUE)
    ),
    uiOutput(paste0(prefix, "_progress_ui")),
    uiOutput(paste0(prefix, "_preview_ui")),
    accordion(
      id = paste0(prefix, "_options_acc"),
      open = FALSE,
      accordion_panel(
        "Options",
        checkboxInput(paste0(prefix, "_do_back"), "Do backward translation", TRUE),
        conditionalPanel(
          condition = paste0("input.", prefix, "_do_back == true"),
          checkboxInput(
            paste0(prefix, "_do_recon"),
            "Do reconciliation / discrepancy check",
            TRUE
          )
        ),
        if (is_batch) numericInput(
          "batch_size", "Batch size (items per LLM call)",
          value = NA, min = 1, step = 1
        ),
        checkboxInput(paste0(prefix, "_debug"), "Verbose debug", FALSE)
      ),
      accordion_panel(
        "Prompt Preview",
        tags$p(
          class = "text-muted small",
          "Read-only preview of the prompts sent to the LLM.",
          "Adjust language, context, and register in the Setup and Translate sidebar."
        ),
        tags$strong("Forward translation prompt"),
        uiOutput(paste0(prefix, "_fwd_prompt_preview")),
        conditionalPanel(
          condition = paste0("input.", prefix, "_do_back == true"),
          tags$strong("Backward translation prompt"),
          uiOutput(paste0(prefix, "_back_prompt_preview")),
          conditionalPanel(
            condition = paste0("input.", prefix, "_do_recon == true"),
            tags$strong("Reconciliation prompt"),
            uiOutput(paste0(prefix, "_recon_prompt_preview"))
          )
        )
      )
    ),
    h5("Results"),
    if (is_batch) uiOutput("batch_sheet_display_selector"),
    DTOutput(paste0(prefix, "_table")),
    accordion(
      open = FALSE,
      accordion_panel(
        "Debug Log",
        verbatimTextOutput(paste0(prefix, "_debug_log"), placeholder = TRUE)
      )
    )
  )
}

ui <- page_navbar(
  title = "LLM Survey Translator",
  theme = bs_theme(bootswatch = "flatly"),
  fillable = FALSE,

  header = tags$style(HTML("
    .shiny-notification {
      position: fixed;
      bottom: 15px;
      right: 15px;
      opacity: 0.95;
      max-width: 350px;
      z-index: 9999;
    }
    .progress-container { margin-bottom: 15px; }
    .progress { height: 30px; margin-bottom: 5px; }
    .progress-bar { font-size: 14px; line-height: 30px; }
    .preview-box {
      background-color: #f8f9fa;
      border: 1px solid #dee2e6;
      border-radius: 4px;
      padding: 15px;
      margin-bottom: 15px;
    }
    .preview-box h5 { margin-top: 0; color: #28a745; }
    .preview-item { margin-bottom: 8px; }
    .preview-label { font-weight: bold; color: #495057; }
    .download-btn > a {
      background-color: #bdf5bd !important;
      border-color: #9de39d !important;
      color: #000 !important;
    }
  ")),

  nav_spacer(),

  nav_panel(
    "Setup",
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Forward Translation"),
        card_body(
          uiOutput("forward_provider_ui"),
          uiOutput("forward_cred_status"),
          uiOutput("forward_model_ui"),
          sliderInput("forward_temp", "Temperature",
                      min = 0, max = 2, value = 0, step = 0.01)
        )
      ),
      card(
        card_header("Backward Translation"),
        card_body(
          uiOutput("back_provider_ui"),
          uiOutput("back_cred_status"),
          uiOutput("back_model_ui"),
          sliderInput("back_temp", "Temperature",
                      min = 0, max = 2, value = 0, step = 0.01)
        )
      ),
      card(
        card_header("Reconciliation"),
        card_body(
          uiOutput("recon_provider_ui"),
          uiOutput("recon_cred_status"),
          uiOutput("recon_model_ui"),
          sliderInput("recon_temp", "Temperature",
                      min = 0, max = 2, value = 0, step = 0.01)
        )
      )
    ),
    card(
      card_body(
        div(
          class = "d-flex gap-2 align-items-center",
          actionButton("test_model", "Test Forward Model Connection",
                       class = "btn-sm btn-info"),
          uiOutput("test_status", inline = TRUE)
        ),
        hr(),
        tags$p(class = "text-muted small mb-0",
          "API keys are read from environment variables. Edit ",
          tags$code("~/.Renviron"), " with ",
          tags$code("usethis::edit_r_environ()"),
          " and restart R. See the ",
          tags$a(
            href = "https://ellmer.tidyverse.org/reference/index.html",
            target = "_blank", "ellmer reference"
          ),
          " for provider-specific variable names."
        )
      )
    )
  ),

  nav_panel(
    "Translate",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        uiOutput("lang_from_ui"),
        uiOutput("lang_to_ui"),
        hr(),
        uiOutput("file_input_ui"),
        uiOutput("col_selector"),
        hr(),
        textAreaInput(
          "instrument_context", "Instrument context (optional)",
          width = "100%", height = "60px", value = "",
          placeholder = "e.g. PHQ-9 depression screening questionnaire"
        ),
        textInput(
          "target_register", "Target register (optional)", value = "",
          placeholder = "e.g. formal clinical language"
        )
      ),
      navset_tab(
        id = "translate_mode",
        nav_panel(
          "Batch",
          mode_panel_ui("batch", "Start Batch Translation", is_batch = TRUE)
        ),
        nav_panel(
          "Item-by-item",
          mode_panel_ui("item", "Start Translation")
        )
      )
    )
  ),

  nav_panel(
    "Help",
    card(
      card_header("How to use LLM Survey Translator"),
      card_body(
        tags$p(
          "This app uses the ",
          tags$a(href = "https://ellmer.tidyverse.org/", target = "_blank", "ellmer"),
          " package to communicate with LLMs. Models are specified in ",
          tags$code("provider/model"), " format:"
        ),
        tags$ul(
          tags$li(tags$code("openai/gpt-4.1"), " - OpenAI GPT-4.1"),
          tags$li(tags$code("openai/gpt-4o-mini"), " - OpenAI GPT-4o Mini"),
          tags$li(tags$code("anthropic/claude-sonnet-4-5-20250929"), " - Claude Sonnet 4.5"),
          tags$li(tags$code("google_gemini/gemini-2.5-flash"), " - Google Gemini 2.5 Flash"),
          tags$li(tags$code("ollama/llama3"), " - Local model via Ollama"),
          tags$li(tags$code("deepseek/deepseek-chat"), " - DeepSeek"),
          tags$li("... and many more (see ",
                  tags$a(
                    href = "https://ellmer.tidyverse.org/reference/index.html",
                    target = "_blank", "ellmer docs"
                  ), ")")
        ),
        tags$p(
          "Set API keys as environment variables in ", tags$code("~/.Renviron"),
          " (e.g. ", tags$code("OPENAI_API_KEY=sk-..."),
          "). See ellmer's documentation for each provider's required variable."
        )
      )
    ),
    card(
      card_header("Excel File Preparation"),
      card_body(
        tags$ul(
          tags$li(strong("One item per row"),
                  ": Each survey item should be in its own row"),
          tags$li(strong("Column with original text"),
                  ": Have a dedicated column containing the items to translate"),
          tags$li(strong("No merged cells"),
                  ": Avoid merged cells in your Excel file"),
          tags$li(strong("Empty rows"),
                  ": Empty rows are automatically skipped during translation"),
          tags$li(strong("Multiple sheets"),
                  ": If your file has multiple sheets, you can select individual sheets or translate all at once"),
          tags$li(strong("File format"), ": Use .xlsx format")
        )
      )
    ),
    card(
      card_header("Translation Steps"),
      card_body(
        tags$ol(
          tags$li(strong("Model Setup"),
                  ": Configure your models in the 'Setup' tab. Ensure API keys are set."),
          tags$li(strong("Choose Translation Mode"), ":"),
          tags$ul(
            tags$li(strong("Item-by-item"),
                    ": Translates each item individually. Best for very long instruments or rate-limited APIs."),
            tags$li(strong("Batch"),
                    ": Translates all items in a single LLM call per stage. Faster and more context-aware.")
          ),
          tags$li(strong("Upload & Configure"),
                  ": Upload Excel in the 'Translate' sidebar, choose ORIGINAL column, adjust prompts if needed."),
          tags$li("Click 'Start Translation' or 'Start Batch Translation'."),
          tags$li("First translation appears as a preview to verify quality."),
          tags$li("Click 'Stop' to halt processing."),
          tags$li("Download the Excel when finished.")
        )
      )
    ),
    card(
      card_header("Troubleshooting"),
      card_body(
        tags$ul(
          tags$li("Authentication errors: Check that your API key environment variable is set correctly. Restart R after editing .Renviron."),
          tags$li("Model not found: Verify the provider/model string matches ellmer's format."),
          tags$li("Token limit errors in Batch mode: Set a smaller batch size in Options (e.g. 50 or 100), switch to Item-by-item, or use a model with higher token limits."),
          tags$li("If something hangs, press Stop. Check the Debug log.")
        )
      )
    )
  ),

  nav_panel(
    "About",
    card(
      card_header("How to cite"),
      card_body(
        tags$p("If you use this package/app, please cite:"),
        tags$pre(
          style = "white-space:pre-wrap;",
          paste0(
            "Kunst, J. R. (2025). LLMTranslate: LLM Survey Translator (Version ",
            packageVersion("LLMTranslate"),
            ") [R package].\nRetrieved from https://CRAN.R-project.org/package=LLMTranslate"
          )
        ),
        tags$p("BibTeX:"),
        tags$pre(
          style = "white-space:pre-wrap;",
          paste0(
            "@Manual{Kunst2025LLMTranslate,\n",
            "  title  = {LLMTranslate: LLM Survey Translator},\n",
            "  author = {Jonas R. Kunst},\n",
            "  year   = {2025},\n",
            "  note   = {R package version ", packageVersion("LLMTranslate"), "},\n",
            "  url    = {https://CRAN.R-project.org/package=LLMTranslate}\n",
            "}"
          )
        ),
        tags$p("Also cite the specific LLMs you used and the translation frameworks (TRAPD, ISPOR).")
      )
    ),
    card(
      card_body(
        tags$p("Automates forward/back translations with optional reconciliation using LLMs."),
        tags$p(
          "Powered by the ",
          tags$a(href = "https://ellmer.tidyverse.org/", target = "_blank", "ellmer"),
          " package for provider-agnostic LLM communication."
        ),
        tags$p("App by Jonas R. Kunst. Modify freely."),
        tags$p("This UI/code was co-developed with assistance from an LLM.")
      )
    )
  )
)
