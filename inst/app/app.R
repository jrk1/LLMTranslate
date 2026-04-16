# ------------------------------------------------------------
# LLM Survey Translator (shiny)
# Powered by ellmer for provider-agnostic LLM communication.
# App by Jonas R. Kunst
# ------------------------------------------------------------

source("global.R", local = TRUE)
source("ui.R", local = TRUE)
source("server-helpers.R", local = TRUE)
source("server-item.R", local = TRUE)
source("server-batch.R", local = TRUE)

server <- function(input, output, session) {
  file_rv <- make_file_rv()
  item_rv <- make_mode_rv()
  batch_rv <- make_mode_rv()

  # ---- Shared file handling ----

  output$file_input_ui <- renderUI({
    fileInput(
      paste0("file_", file_rv$file_reset),
      "Upload Excel", accept = ".xlsx"
    )
  })

  output$col_selector <- renderUI({
    build_col_selector(
      input, file_rv,
      paste0("file_", file_rv$file_reset),
      "sheet_select", "orig_col"
    )
  })

  observeEvent(
    input[[paste0("file_", isolate(file_rv$file_reset))]],
    {
      load_file_data(
        input, file_rv,
        paste0("file_", file_rv$file_reset),
        "sheet_select"
      )
    },
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  observeEvent(
    input$sheet_select,
    {
      load_file_data(
        input, file_rv,
        paste0("file_", file_rv$file_reset),
        "sheet_select"
      )
    },
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # ---- Language inputs ----

  output$lang_from_ui <- renderUI({
    make_lang_ui("lang_from", "Translate FROM (language)", "English")
  })
  output$lang_to_ui <- renderUI({
    make_lang_ui("lang_to", "Translate TO (language)", "German")
  })

  # ---- Model Setup tab ----

  auto_provider <- default_provider(ALL_PROVIDERS)

  output$forward_provider_ui <- renderUI({
    selectInput("forward_provider", "Provider",
                choices = ALL_PROVIDERS, selected = auto_provider)
  })
  output$back_provider_ui <- renderUI({
    selectInput("back_provider", "Provider",
                choices = ALL_PROVIDERS, selected = auto_provider)
  })
  output$recon_provider_ui <- renderUI({
    selectInput("recon_provider", "Provider",
                choices = ALL_PROVIDERS, selected = auto_provider)
  })

  output$forward_cred_status <- renderUI({
    req(input$forward_provider)
    render_cred_badge(input$forward_provider)
  })
  output$back_cred_status <- renderUI({
    req(input$back_provider)
    render_cred_badge(input$back_provider)
  })
  output$recon_cred_status <- renderUI({
    req(input$recon_provider)
    render_cred_badge(input$recon_provider)
  })

  output$forward_model_ui <- renderUI({
    req(input$forward_provider)
    make_model_ui(input$forward_provider, "forward_model")
  })
  output$back_model_ui <- renderUI({
    req(input$back_provider)
    make_model_ui(input$back_provider, "back_model")
  })
  output$recon_model_ui <- renderUI({
    req(input$recon_provider)
    make_model_ui(input$recon_provider, "recon_model")
  })

  observeEvent(input$test_model, {
    tryCatch(
      {
        model_str <- full_model(input, "forward_provider", "forward_model")
        req(nzchar(model_str))
        .pkg$llm_call(model_str, "Reply with just the word 'OK'.", temperature = 0)

        output$test_status <- renderUI({
          tags$span(
            class = "text-success fw-bold",
            HTML(paste("&#10003; Connection to", model_str, "successful"))
          )
        })
        showNotification(
          paste("Connection to", model_str, "successful!"),
          type = "message"
        )
      },
      error = function(e) {
        output$test_status <- renderUI({
          tags$span(
            class = "text-danger fw-bold",
            HTML(paste("&#10007; Error:", e$message))
          )
        })
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # ---- Delegate to module servers ----
  item_server(input, output, session, file_rv, item_rv)
  batch_server(input, output, session, file_rv, batch_rv)
}

shiny::shinyApp(ui = ui, server = server)
