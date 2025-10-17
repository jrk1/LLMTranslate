# ------------------------------------------------------------
# LLM Survey Translator (shiny)
# Revision r15a – add GPT-5 series (gpt-5 / gpt-5-mini / gpt-5-nano),
# reasoning effort "minimal", and make temperature ignored for reasoning models.
# App by Jonas R. Kunst
# ------------------------------------------------------------

# ---- Packages (UI-facing) ----
library(shiny)
library(DT)
library(readxl)
library(writexl)
library(openxlsx)
library(httr2)
library(jsonlite)
library(glue)
library(later)

# ---- Pull internal helpers/objects from package namespace ----
grab <- function(x) getFromNamespace(x, "LLMTranslate")

MODEL_SPEC                         <- grab("MODEL_SPEC")
NORMALIZE_MAP                      <- grab("NORMALIZE_MAP")
normalize_model                    <- grab("normalize_model")
get_spec                           <- grab("get_spec")
coalesce_chr                       <- grab("coalesce_chr")
`%null%`                           <- grab("%null%")
make_logger                        <- grab("make_logger")
perform_req                        <- grab("perform_req")
strip_AB_prefix                    <- grab("strip_AB_prefix")
strip_code_fences                  <- grab("strip_code_fences")
parse_recon_output                 <- grab("parse_recon_output")
call_openai_chat                   <- grab("call_openai_chat")
call_openai_reasoning_responses    <- grab("call_openai_reasoning_responses")
call_gemini_chat                   <- grab("call_gemini_chat")
call_claude_chat                   <- grab("call_claude_chat")
llm_call                           <- grab("llm_call")

# helper: detect models that should be treated as "reasoning" (ignore temperature, allow effort)
is_reasoning_model <- function(m) {
  if (is.null(m)) return(FALSE)
  # normalize to lower
  m <- tolower(m)
  grepl("^gpt-5", m) || grepl("^o[13]\\b", m)
}

# ---- Default prompts (editable in UI) ----
DEFAULT_FORWARD <- paste(
  "You are a professional survey translator working within TRAPD/ISPOR best practices.",
  "Goal: Produce a conceptually equivalent {to_lang} version of the item below.",
  "Constraints:",
  "- Preserve meaning, intent, item polarity (negations), numbers, quantifiers, modality, and time references. Do not introduce changes if not needed. A translation as directly as possible is preferable as long as it is culturally meaningful.",
  "- Avoid culture-bound idioms/metaphors unless an equivalent exists.",
  "- Keep reading level and tone comparable to the source.",
  "- Output ONLY the final {to_lang} item text (no quotes, no explanations).",
  "",
  "ITEM ({from_lang}): {text}",
  sep = "\n"
)

DEFAULT_BACK <- paste(
  "You are performing a blind back-translation as part of a TRAPD/ISPOR quality check.",
  "Goal: Render the {to_lang} item back into {from_lang} as literally as possible while remaining grammatical.",
  "Rules:",
  "- Do NOT try to improve wording or guess the original; reflect exactly what the {to_lang} item says.",
  "- Preserve polarity, quantifiers, modality, and tense.",
  "- No added comments, brackets, or explanations.",
  "- Output ONLY the {from_lang} text (no quotes).",
  "",
  "ITEM ({to_lang}): {text}",
  sep = "\n"
)

DEFAULT_RECON_UI <- paste(
  "You are reconciling a survey translation (TRAPD/ISPOR step).",
  "Inputs:",
  "1) ORIGINAL (in {from_lang})",
  "2) FORWARD (in {to_lang})",
  "3) BACK (in {from_lang})",
  "Tasks:",
  "- Compare ORIGINAL vs BACK to detect meaning shifts, omissions, or added nuances.",
  "- Accept FORWARD if conceptually equivalent and natural in {to_lang}.",
  "- If revision is needed, adjust FORWARD to match ORIGINAL meaning precisely while keeping tone/register and length roughly similar.",
  "Return a JSON object with exactly these keys:",
  "{{\"revised\": \"<revised {to_lang} item only>\", \"explanation\": \"<one short sentence on any change>\"}}.",
  "No other text.",
  sep = "\n"
)

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-notification {
        position: fixed;
        bottom: 15px;
        right: 15px;
        opacity: 0.95;
        max-width: 350px;
        z-index: 9999;
      }
      .top-buttons{
        display:flex;
        gap:8px;
        align-items:center;
        margin-bottom:10px;
      }
      #download_btn { margin-left:auto; }
      #download_btn > button {
        background-color:#bdf5bd !important;
        border-color:#9de39d !important;
        color:#000 !important;
      }
      .temp-hint { font-size: 12px; color: #666; margin-top: -6px; margin-bottom: 8px; }
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
      .test-btn { margin-top: -10px; margin-bottom: 10px; font-size: 12px; }
      .api-status-success { color: #155724; font-weight: bold; font-size: 13px; }
      .api-status-error { color: #721c24; font-weight: bold; font-size: 13px; }
    "))
  ),
  titlePanel("LLM Survey Translator"),
  tabsetPanel(
    id = "tabs",
    tabPanel("API Keys & Models",
             br(),
             fluidRow(
               column(4,
                      h4("OpenAI"),
                      textInput("openai_key", "OpenAI API Key",
                                value = Sys.getenv("OPENAI_API_KEY"), width = "100%"),
                      actionButton("test_openai", "Test Connection", class = "btn-sm test-btn"),
                      uiOutput("openai_status")
               ),
               column(4,
                      h4("Gemini"),
                      textInput("gemini_key", "Gemini API Key",
                                value = Sys.getenv("GEMINI_API_KEY"), width = "100%"),
                      actionButton("test_gemini", "Test Connection", class = "btn-sm test-btn"),
                      uiOutput("gemini_status")
               ),
               column(4,
                      h4("Claude"),
                      textInput("claude_key", "Claude API Key",
                                value = Sys.getenv("CLAUDE_API_KEY"), width = "100%"),
                      actionButton("test_claude", "Test Connection", class = "btn-sm test-btn"),
                      uiOutput("claude_status")
               )
             ),
             hr(),
             h4("Model Selection"),
             fluidRow(
               column(6,
                      selectInput(
                        "forward_model", "Forward translation model",
                        choices  = MODEL_SPEC$name,
                        selected = if ("gpt-5-mini" %in% MODEL_SPEC$name) "gpt-5-mini" else
                          if ("gpt-4o-mini" %in% MODEL_SPEC$name) "gpt-4o-mini" else
                            MODEL_SPEC$name[[1]]
                      ),
                      sliderInput("forward_temp", "Forward model temperature",
                                  min = 0, max = 1, value = 0, step = 0.01),
                      uiOutput("forward_temp_hint")
               ),
               column(6,
                      selectInput(
                        "back_model", "Backward translation model",
                        choices  = MODEL_SPEC$name,
                        selected = if ("gpt-5-mini" %in% MODEL_SPEC$name) "gpt-5-mini" else
                          if ("gpt-4.1-mini" %in% MODEL_SPEC$name) "gpt-4.1-mini" else
                            MODEL_SPEC$name[[1]]
                      ),
                      sliderInput("back_temp", "Backward model temperature",
                                  min = 0, max = 2, value = 0, step = 0.01),
                      uiOutput("back_temp_hint")
               )
             ),
             hr(),
             fluidRow(
               column(6,
                      selectInput(
                        "recon_model", "Reconciliation model (3rd call)",
                        choices  = MODEL_SPEC$name,
                        selected = if ("gpt-5" %in% MODEL_SPEC$name) "gpt-5" else
                          if ("gpt-4.1-mini" %in% MODEL_SPEC$name) "gpt-4.1-mini" else
                            MODEL_SPEC$name[[1]]
                      ),
                      sliderInput("recon_temp", "Reconciliation model temperature",
                                  min = 0, max = 1, value = 0, step = 0.01),
                      uiOutput("recon_temp_hint")
               ),
               column(6,
                      selectInput(
                        "reasoning_effort", "Reasoning effort (GPT-5 / o-series)",
                        choices = c("minimal","low","medium","high"),
                        selected = "medium"
                      ),
                      helpText("Used for GPT-5 and o-series. Temperature is ignored.")
               )
             )
    ),
    tabPanel("Translation",
             sidebarLayout(
               sidebarPanel(
                 textInput("lang_from","Translate FROM (language)", value = "English"),
                 textInput("lang_to",  "Translate TO (language)",   value = "German"),
                 uiOutput("file_input_ui"),
                 uiOutput("col_selector"),
                 textAreaInput("forward_prompt", "Forward translation prompt",
                               width = "100%", height = "170px", value = DEFAULT_FORWARD),
                 checkboxInput("do_back","Do backward translation", TRUE),
                 conditionalPanel(
                   condition = "input.do_back == true",
                   textAreaInput("back_prompt","Backward translation prompt",
                                 width = "100%", height = "150px", value = DEFAULT_BACK),
                   checkboxInput("do_recon","Do reconciliation / discrepancy check", TRUE),
                   conditionalPanel(
                     condition = "input.do_recon == true",
                     textAreaInput("recon_prompt","Reconciliation prompt",
                                   width = "100%", height = "240px", value = DEFAULT_RECON_UI)
                   )
                 ),
                 checkboxInput("debug","Verbose debug (prints requests/responses)", FALSE)
               ),
               mainPanel(
                 div(class="top-buttons",
                     actionButton("run",  "Start Translation", class = "btn-primary"),
                     actionButton("stop", "Stop",             class = "btn-danger"),
                     actionButton("reset", "Reset",           class = "btn-warning"),
                     uiOutput("download_ui", inline = TRUE)
                 ),
                 uiOutput("progress_ui"),
                 uiOutput("preview_ui"),
                 h4("Preview / Results"),
                 DTOutput("table"),
                 hr(),
                 h4("Debug log"),
                 verbatimTextOutput("debug_log", placeholder = TRUE)
               )
             )
    ),
    tabPanel("API Keys Guide",
             br(),
             h3("How to Obtain API Keys"),
             tags$div(
               h4("OpenAI API Key"),
               tags$ol(
                 tags$li("Go to ", tags$a(href="https://platform.openai.com/api-keys", target="_blank", "platform.openai.com/api-keys")),
                 tags$li("Sign in or create an account"),
                 tags$li("Click \"Create new secret key\""),
                 tags$li("Name your key (e.g., \"LLMTranslate\") and click \"Create secret key\""),
                 tags$li("Copy the key immediately (it won't be shown again)"),
                 tags$li("Add billing information at ", tags$a(href="https://platform.openai.com/account/billing", target="_blank", "Billing settings")),
                 tags$li("Paste the key into the OpenAI API Key field in the \"API Keys & Models\" tab")
               ),
               p(strong("Note:"), " OpenAI requires prepaid credits. Monitor your usage at ",
                 tags$a(href="https://platform.openai.com/usage", target="_blank", "Usage dashboard"), "."),
               hr(),

               h4("Google Gemini API Key"),
               tags$ol(
                 tags$li("Go to ", tags$a(href="https://aistudio.google.com/app/apikey", target="_blank", "aistudio.google.com/app/apikey")),
                 tags$li("Sign in with your Google account"),
                 tags$li("Click \"Create API key\""),
                 tags$li("Select an existing Google Cloud project or create a new one"),
                 tags$li("Copy the generated API key"),
                 tags$li("Paste the key into the Gemini API Key field in the \"API Keys & Models\" tab")
               ),
               p(strong("Note:"), " Gemini offers a free tier with rate limits. See ",
                 tags$a(href="https://ai.google.dev/pricing", target="_blank", "pricing details"), "."),
               hr(),

               h4("Anthropic Claude API Key"),
               tags$ol(
                 tags$li("Go to ", tags$a(href="https://console.anthropic.com/", target="_blank", "console.anthropic.com")),
                 tags$li("Sign in or create an account"),
                 tags$li("Navigate to \"API Keys\" in the left sidebar"),
                 tags$li("Click \"Create Key\""),
                 tags$li("Name your key (e.g., \"LLMTranslate\") and set permissions"),
                 tags$li("Copy the generated key immediately"),
                 tags$li("Add credits at ", tags$a(href="https://console.anthropic.com/settings/billing", target="_blank", "Billing settings")),
                 tags$li("Paste the key into the Claude API Key field in the \"API Keys & Models\" tab")
               ),
               p(strong("Note:"), " Claude requires prepaid credits. New users may receive initial credits. Monitor usage in the console."),
               hr(),

               h4("Security Best Practices"),
               tags$ul(
                 tags$li("Never share your API keys publicly or commit them to version control"),
                 tags$li("Store keys in environment variables (OPENAI_API_KEY, GEMINI_API_KEY, CLAUDE_API_KEY)"),
                 tags$li("Rotate keys periodically for security"),
                 tags$li("Set usage limits in provider dashboards to control costs"),
                 tags$li("Monitor usage regularly to detect unexpected API calls")
               )
             )
    ),
    tabPanel("Help",
             br(),
             h3("How to use LLM Survey Translator"),
             tags$ol(
               tags$li(strong("API Keys & Models"), ": Enter keys (see \"API Keys Guide\" tab). Pick models for forward, back, reconcile. Use \"Test Connection\" to verify keys work."),
               tags$li(strong("Translation"), ": Upload Excel, choose ORIGINAL column, adjust prompts if needed."),
               tags$li("Click 'Start Translation'. Progress bar shows current progress. Status toasts show: \u2192 forward, \u2190 back, \u21bb reconciling."),
               tags$li("First translation appears as a preview to verify quality."),
               tags$li("Click 'Stop' to halt before the next call/item. You can resume later."),
               tags$li("Download the Excel with new columns when finished. File includes Model Selection Log and Prompt Log sheets.")
             ),
             h4("Prompts"),
             p("Defaults follow TRAPD/ISPOR recommendations. Adapt to your domain."),
             h4("Troubleshooting"),
             tags$ul(
               tags$li("HTTP 400/404: model or endpoint mismatch. Try another model or test API key connection."),
               tags$li("If something hangs, press Stop. Check the Debug log."),
               tags$li("Ensure Excel column names are correct and items are not empty."),
               tags$li("For large batches (500+ items), expect longer processing times.")
             )
    ),
    tabPanel(
      "Citation",
      br(),
      h3("How to cite"),
      p("If you use this package/app, please cite:"),
      tags$pre(style = "white-space:pre-wrap;",
               "Kunst, J. R. (2025). LLMTranslate: LLM Survey Translator (Version 0.3.0) [R package].
Retrieved from https://CRAN.R-project.org/package=LLMTranslate"),
      p("BibTeX:"),
      tags$pre(style = "white-space:pre-wrap;",
               "@Manual{Kunst2025LLMTranslate,
  title  = {LLMTranslate: LLM Survey Translator},
  author = {Jonas R. Kunst},
  year   = {2025},
  note   = {R package version 0.3.0},
  url    = {https://CRAN.R-project.org/package=LLMTranslate}
}"),
      p("Also cite the specific LLMs you used and the translation frameworks (TRAPD, ISPOR).")
    ),
    tabPanel("About",
             br(),
             p("Automates forward/back translations with optional reconciliation using LLMs."),
             p("App by Jonas R. Kunst. Modify freely."),
             p("This UI/code was co-developed with assistance from an LLM.")
    )
  )
)

# ---- Server ----
server <- function(input, output, session){
  rv <- reactiveValues(
    df = NULL, result = NULL, log = character(),
    running = FALSE, stop = FALSE,
    state = NULL, notif_id = NULL,
    preview = NULL, progress_pct = 0, progress_text = "",
    file_reset = 0
  )

  # dynamic hints under temperature sliders when a reasoning model is chosen
  output$forward_temp_hint <- renderUI({
    m <- normalize_model(input$forward_model)
    if (is_reasoning_model(m)) div(class="temp-hint","Temperature is ignored for GPT-5 / o-series.")
  })
  output$back_temp_hint <- renderUI({
    m <- normalize_model(input$back_model)
    if (is_reasoning_model(m)) div(class="temp-hint","Temperature is ignored for GPT-5 / o-series.")
  })
  output$recon_temp_hint <- renderUI({
    m <- normalize_model(input$recon_model)
    if (is_reasoning_model(m)) div(class="temp-hint","Temperature is ignored for GPT-5 / o-series.")
  })

  # API key testing
  observeEvent(input$test_openai, {
    req(input$openai_key)
    tryCatch({
      call_openai_chat("gpt-4o-mini", "Test", 0, input$openai_key, function(...){})
      output$openai_status <- renderUI({
        tags$span(class="api-status-success", HTML("&#10003; Connection successful"))
      })
      showNotification("OpenAI connection successful!", type = "message")
    }, error = function(e){
      output$openai_status <- renderUI({
        tags$span(class="api-status-error", HTML(paste("&#10007; Error:", e$message)))
      })
      showNotification(paste("OpenAI error:", e$message), type = "error")
    })
  })

  observeEvent(input$test_gemini, {
    req(input$gemini_key)
    tryCatch({
      call_gemini_chat("gemini-2.5-flash", "Test", 0, input$gemini_key, function(...){})
      output$gemini_status <- renderUI({
        tags$span(class="api-status-success", HTML("&#10003; Connection successful"))
      })
      showNotification("Gemini connection successful!", type = "message")
    }, error = function(e){
      output$gemini_status <- renderUI({
        tags$span(class="api-status-error", HTML(paste("&#10007; Error:", e$message)))
      })
      showNotification(paste("Gemini error:", e$message), type = "error")
    })
  })

  observeEvent(input$test_claude, {
    req(input$claude_key)
    tryCatch({
      call_claude_chat("claude-sonnet-4-5-20250929", "Test", 0, input$claude_key, function(...){})
      output$claude_status <- renderUI({
        tags$span(class="api-status-success", HTML("&#10003; Connection successful"))
      })
      showNotification("Claude connection successful!", type = "message")
    }, error = function(e){
      output$claude_status <- renderUI({
        tags$span(class="api-status-error", HTML(paste("&#10007; Error:", e$message)))
      })
      showNotification(paste("Claude error:", e$message), type = "error")
    })
  })

  # Progress bar
  output$progress_ui <- renderUI({
    req(rv$running, rv$progress_text)
    div(class="progress-container",
        div(class="progress",
            div(class="progress-bar progress-bar-striped progress-bar-animated",
                role="progressbar",
                style=paste0("width: ", rv$progress_pct, "%"),
                rv$progress_text)
        )
    )
  })

  # Preview of first translation
  output$preview_ui <- renderUI({
    req(rv$preview)
    div(class="preview-box",
        h5(HTML("&#10003; First Translation Preview (verify quality and stop if needed before remaining items are processed)")),
        div(class="preview-item",
            span(class="preview-label", "Original: "), rv$preview$original),
        div(class="preview-item",
            span(class="preview-label", "Forward: "), rv$preview$forward),
        if (!is.null(rv$preview$back))
          div(class="preview-item",
              span(class="preview-label", "Back: "), rv$preview$back),
        if (!is.null(rv$preview$reconciled))
          div(class="preview-item",
              span(class="preview-label", "Reconciled: "), rv$preview$reconciled)
    )
  })

  update_status <- function(msg, type = "message"){
    old_id <- isolate(rv$notif_id)
    if (!is.null(old_id)){
      try(removeNotification(old_id, session = session), silent = TRUE)
    }
    id <- showNotification(msg, type = type,
                           duration = NULL, closeButton = FALSE,
                           session = session)
    isolate(rv$notif_id <- id)
  }

  # Dynamic file input to allow reset
  output$file_input_ui <- renderUI({
    fileInput(paste0("file_", rv$file_reset), "Upload Excel", accept = c(".xlsx",".xls"))
  })

  output$col_selector <- renderUI({
    file_input <- input[[paste0("file_", rv$file_reset)]]
    req(file_input)
    df_head <- read_excel(file_input$datapath, n_max = 1)
    selectInput("orig_col", "Column with ORIGINAL item", choices = names(df_head))
  })

  observe({
    file_input <- input[[paste0("file_", rv$file_reset)]]
    if (!is.null(file_input)) {
      rv$df     <- read_excel(file_input$datapath)
      rv$result <- NULL
    }
  })

  observeEvent(input$do_back, {
    if (!isTRUE(input$do_back)){
      updateCheckboxInput(session, "do_recon", value = FALSE)
    }
  })

  observeEvent(input$stop, {
    rv$stop <- TRUE
    update_status("STOP requested… finishing current call.", "warning")
  })

  observeEvent(input$reset, {
    rv$df <- NULL
    rv$result <- NULL
    rv$log <- character()
    rv$running <- FALSE
    rv$stop <- FALSE
    rv$state <- NULL
    rv$notif_id <- NULL
    rv$preview <- NULL
    rv$progress_pct <- 0
    rv$progress_text <- ""
    rv$file_reset <- rv$file_reset + 1  # Increment to reset file input

    # Reset input fields
    updateTextInput(session, "lang_from", value = "English")
    updateTextInput(session, "lang_to", value = "German")
    updateTextAreaInput(session, "forward_prompt", value = DEFAULT_FORWARD)
    updateTextAreaInput(session, "back_prompt", value = DEFAULT_BACK)
    updateTextAreaInput(session, "recon_prompt", value = DEFAULT_RECON_UI)

    showNotification("Reset complete. Upload a new file to start.", type = "message")
  })

  output$download_ui <- renderUI({
    req(rv$result)
    div(id="download_btn", downloadButton("download","Download Excel"))
  })

  output$download <- downloadHandler(
    filename = function() paste0("translated_", Sys.Date(), ".xlsx"),
    content  = function(file){
      req(rv$result)
      cfg <- rv$state

      # Create model selection log
      model_log <- data.frame(
        Step = c("Forward Translation", "Backward Translation", "Reconciliation"),
        Model = c(
          if (!is.null(cfg)) cfg$f_model else input$forward_model,
          if (!is.null(cfg)) cfg$b_model else input$back_model,
          if (!is.null(cfg)) cfg$r_model else input$recon_model
        ),
        Temperature = c(
          if (!is.null(cfg)) ifelse(is.null(cfg$f_temp), "N/A (reasoning model)", cfg$f_temp) else input$forward_temp,
          if (!is.null(cfg)) ifelse(is.null(cfg$b_temp), "N/A (reasoning model)", cfg$b_temp) else input$back_temp,
          if (!is.null(cfg)) ifelse(is.null(cfg$r_temp), "N/A (reasoning model)", cfg$r_temp) else input$recon_temp
        ),
        stringsAsFactors = FALSE
      )

      # Create prompt log
      prompt_log <- data.frame(
        Step = c("Forward Translation", "Backward Translation", "Reconciliation"),
        Prompt = c(
          if (!is.null(cfg)) cfg$forward_prompt else input$forward_prompt,
          if (!is.null(cfg)) cfg$back_prompt else input$back_prompt,
          if (!is.null(cfg)) cfg$recon_prompt else input$recon_prompt
        ),
        stringsAsFactors = FALSE
      )

      # Create workbook with multiple sheets
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Translation Results")
      openxlsx::writeData(wb, "Translation Results", rv$result)
      openxlsx::addWorksheet(wb, "Model Selection Log")
      openxlsx::writeData(wb, "Model Selection Log", model_log)
      openxlsx::addWorksheet(wb, "Prompt Log")
      openxlsx::writeData(wb, "Prompt Log", prompt_log)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$debug_log <- renderText(paste(rv$log, collapse = "\n"))

  output$table <- renderDT({
    dat <- if (is.null(rv$result)) rv$df else rv$result
    req(dat)
    datatable(dat, options = list(pageLength = 20, scrollX = TRUE))
  })

  observeEvent(input$run, {
    req(rv$df, input$orig_col)

    # Warn for large batches
    n_items <- nrow(rv$df)
    if (n_items >= 500 && (is.null(rv$state) || rv$state$i == 1)) {
      showNotification(
        paste0("Processing ", n_items, " items. This may take some time. Please be patient."),
        type = "warning", duration = 10
      )
    }

    # Resume or start fresh
    resume <- !is.null(rv$state) && rv$state$i <= rv$state$n
    if (!resume) {
      rv$stop    <- FALSE
      rv$running <- TRUE
      rv$log     <- character()
      rv$preview <- NULL
    } else {
      rv$stop    <- FALSE
      rv$running <- TRUE
    }

    logger     <- make_logger(rv, isTRUE(input$debug))
    logger(if (resume) "---- RUN RESUME ----" else "---- RUN START ----")

    f_model <- normalize_model(input$forward_model)
    b_model <- normalize_model(input$back_model)
    r_model <- normalize_model(input$recon_model)
    for (m in c(f_model, b_model, r_model)){
      if (!m %in% MODEL_SPEC$name){
        showNotification(paste("Unsupported model:", m), type = "error", session = session)
        logger("Unsupported model:", m)
        return(NULL)
      }
    }

    # For reasoning models (GPT-5 / o-series), temperature must not be sent.
    f_temp_eff <- if (is_reasoning_model(f_model)) NULL else input$forward_temp
    b_temp_eff <- if (is_reasoning_model(b_model)) NULL else input$back_temp
    r_temp_eff <- if (is_reasoning_model(r_model)) NULL else input$recon_temp

    if (!resume) {
      cfg <- list(
        from_lang = input$lang_from,
        to_lang   = input$lang_to,
        forward_prompt = input$forward_prompt,
        back_prompt    = input$back_prompt,
        recon_prompt   = input$recon_prompt,
        do_back  = isTRUE(input$do_back),
        do_recon = isTRUE(input$do_back) && isTRUE(input$do_recon),
        openai_key = input$openai_key,
        gemini_key = input$gemini_key,
        claude_key = input$claude_key,
        effort     = input$reasoning_effort,
        f_temp = f_temp_eff,
        b_temp = b_temp_eff,
        r_temp = r_temp_eff,
        f_model = f_model,
        b_model = b_model,
        r_model = r_model,
        orig_col = input$orig_col,
        df       = rv$df,
        n        = nrow(rv$df),
        i        = 1,
        stage    = "fwd",
        fwd_col   = paste0("Forward_", input$lang_to),
        back_col  = if (isTRUE(input$do_back)) paste0("Back_", input$lang_from) else NULL,
        recon_col = if (isTRUE(input$do_back) && isTRUE(input$do_recon)) paste0("Reconciled_", input$lang_to) else NULL,
        change_col= if (isTRUE(input$do_back) && isTRUE(input$do_recon)) "Recon_Explanation" else NULL,
        forward_out = NULL,
        back_out    = NULL
      )
      for (cc in c(cfg$fwd_col, cfg$back_col, cfg$recon_col, cfg$change_col))
        if (!is.null(cc)) cfg$df[[cc]] <- NA_character_

      rv$state <- cfg
      update_status("Starting…", "message")
    } else {
      update_status("Resuming…", "message")
    }

    process_step <- NULL
    process_step <- function(){
      cfg <- isolate(rv$state)
      if (is.null(cfg)) return()

      if (isolate(rv$stop)){
        rv$running <- FALSE
        rv$result  <- cfg$df
        update_status("Stopped.", "warning")
        logger("---- RUN STOPPED ----")
        isolate(rv$state <- NULL)
        return()
      }

      if (cfg$i > cfg$n){
        rv$running <- FALSE
        rv$result  <- cfg$df
        update_status("Done.", "message")
        logger("---- RUN END ----")
        isolate(rv$state <- NULL)
        return()
      }

      original_text <- as.character(cfg$df[[cfg$orig_col]][cfg$i])
      if (is.na(original_text) || !nzchar(original_text)){
        logger("Item", cfg$i, "empty; skipping")
        cfg$i    <- cfg$i + 1
        cfg$stage<- "fwd"
        rv$state <- cfg
        later(process_step, 0.001)
        return()
      }

      stage_label <- switch(cfg$stage,
                            fwd   = "\u2192 forward translating",
                            back  = "\u2190 back translating",
                            recon = "\u21bb reconciling",
                            "next_item" = "done"
      )
      update_status(sprintf("%s | Item %d/%d", stage_label, cfg$i, cfg$n), "message")
      logger("Processing item", cfg$i, "stage", cfg$stage)

      # Update progress bar
      isolate({
        rv$progress_pct <- round((cfg$i - 1) / cfg$n * 100)
        rv$progress_text <- sprintf("Item %d of %d (%d%%)", cfg$i, cfg$n, rv$progress_pct)
      })

      tryCatch({
        if (cfg$stage == "fwd"){
          f_prompt <- glue_data(
            list(text = original_text, from_lang = cfg$from_lang, to_lang = cfg$to_lang),
            cfg$forward_prompt
          )
          out <- llm_call(cfg$f_model, f_prompt, cfg$f_temp,
                          cfg$openai_key, cfg$gemini_key, cfg$claude_key, logger, cfg$effort)
          cfg$df[[cfg$fwd_col]][cfg$i] <- out
          cfg$forward_out <- out
          cfg$stage <- if (cfg$do_back) "back" else if (cfg$do_recon) "recon" else "next_item"

        } else if (cfg$stage == "back"){
          b_prompt <- glue_data(
            list(text = cfg$forward_out, from_lang = cfg$from_lang, to_lang = cfg$to_lang),
            cfg$back_prompt
          )
          out <- llm_call(cfg$b_model, b_prompt, cfg$b_temp,
                          cfg$openai_key, cfg$gemini_key, cfg$claude_key, logger, cfg$effort)
          cfg$df[[cfg$back_col]][cfg$i] <- out
          cfg$back_out <- out
          cfg$stage <- if (cfg$do_recon) "recon" else "next_item"

        } else if (cfg$stage == "recon"){
          r_prompt_body <- glue_data(
            list(from_lang = cfg$from_lang, to_lang = cfg$to_lang),
            cfg$recon_prompt
          )
          recon_full_prompt <- paste(
            r_prompt_body,
            "\n---\nORIGINAL:\n", original_text,
            if (cfg$do_back) paste0("\nBACK:\n", cfg$back_out) else "",
            "\nFORWARD:\n", cfg$forward_out,
            sep = ""
          )
          out <- llm_call(cfg$r_model, recon_full_prompt, cfg$r_temp,
                          cfg$openai_key, cfg$gemini_key, cfg$claude_key, logger, cfg$effort)
          parsed <- parse_recon_output(out)
          cfg$df[[cfg$recon_col]][cfg$i]  <- parsed$revised
          cfg$df[[cfg$change_col]][cfg$i] <- parsed$explanation
          cfg$stage <- "next_item"

        } else if (cfg$stage == "next_item"){
          # Capture preview for first item
          if (cfg$i == 1 && is.null(isolate(rv$preview))) {
            isolate({
              rv$preview <- list(
                original = original_text,
                forward = cfg$df[[cfg$fwd_col]][cfg$i],
                back = if (!is.null(cfg$back_col)) cfg$df[[cfg$back_col]][cfg$i] else NULL,
                reconciled = if (!is.null(cfg$recon_col)) cfg$df[[cfg$recon_col]][cfg$i] else NULL
              )
            })
          }

          cfg$i    <- cfg$i + 1
          cfg$stage<- "fwd"
        }

        rv$state <- cfg
        later(process_step, 0.001)

      }, error = function(e){
        logger("ERROR:", e$message)
        if (cfg$stage == "fwd")       cfg$df[[cfg$fwd_col]][cfg$i]   <- paste0("ERROR: ", e$message)
        else if (cfg$stage == "back") cfg$df[[cfg$back_col]][cfg$i]  <- paste0("ERROR: ", e$message)
        else if (cfg$stage == "recon"){
          cfg$df[[cfg$recon_col]][cfg$i]  <- paste0("ERROR: ", e$message)
          cfg$df[[cfg$change_col]][cfg$i] <- ""
        }
        cfg$stage <- "next_item"
        rv$state  <- cfg
        later(process_step, 0.001)
      })
    }

    later(process_step, 0.001)
  })
}

# For completeness
app_ui     <- ui
app_server <- server

# Must return a shiny.appobj:
shiny::shinyApp(ui = ui, server = server)
