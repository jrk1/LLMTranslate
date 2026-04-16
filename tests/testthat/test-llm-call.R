describe("llm_call", {
  lc <- grab("llm_call")

  it("passes model name and prompt to ellmer::chat", {
    captured_args <- NULL
    mock_chat_obj <- list(
      chat = function(prompt) {
        captured_args$prompt <<- prompt
        "mocked response"
      }
    )
    local_mocked_bindings(
      chat = function(...) {
        captured_args <<- list(...)
        mock_chat_obj
      },
      .package = "ellmer"
    )
    result <- lc("openai/gpt-4.1", "hello world")
    expect_equal(captured_args$name, "openai/gpt-4.1")
    expect_equal(captured_args$prompt, "hello world")
    expect_equal(result, "mocked response")
  })

  it("passes temperature via ellmer::params when provided", {
    captured_args <- NULL
    mock_chat_obj <- list(chat = function(prompt) "ok")
    local_mocked_bindings(
      chat = function(...) {
        captured_args <<- list(...)
        mock_chat_obj
      },
      params = function(...) list(...),
      .package = "ellmer"
    )
    lc("openai/gpt-4.1", "test", temperature = 0.5)
    expect_equal(captured_args$params$temperature, 0.5)
  })

  it("omits params when temperature is NULL", {
    captured_args <- NULL
    mock_chat_obj <- list(chat = function(prompt) "ok")
    local_mocked_bindings(
      chat = function(...) {
        captured_args <<- list(...)
        mock_chat_obj
      },
      .package = "ellmer"
    )
    lc("openai/gpt-4.1", "test", temperature = NULL)
    expect_null(captured_args$params)
  })

  it("sets echo to none", {
    captured_args <- NULL
    mock_chat_obj <- list(chat = function(prompt) "ok")
    local_mocked_bindings(
      chat = function(...) {
        captured_args <<- list(...)
        mock_chat_obj
      },
      .package = "ellmer"
    )
    lc("openai/gpt-4.1", "test")
    expect_equal(captured_args$echo, "none")
  })

  it("wraps chat creation errors with model name", {
    local_mocked_bindings(
      chat = function(...) stop("connection refused"),
      .package = "ellmer"
    )
    expect_error(
      lc("badprovider/model", "test"),
      "badprovider/model"
    )
  })

  it("wraps chat$chat errors with model name", {
    mock_chat_obj <- list(
      chat = function(prompt) stop("rate limit exceeded")
    )
    local_mocked_bindings(
      chat = function(...) mock_chat_obj,
      .package = "ellmer"
    )
    expect_error(
      lc("openai/gpt-4.1", "test"),
      "openai/gpt-4.1"
    )
    expect_error(
      lc("openai/gpt-4.1", "test"),
      "rate limit"
    )
  })

  it("calls logger with model and prompt info", {
    mock_chat_obj <- list(chat = function(prompt) "ok")
    local_mocked_bindings(
      chat = function(...) mock_chat_obj,
      .package = "ellmer"
    )
    logs <- character()
    logger <- function(...) logs <<- c(logs, paste(..., collapse = " "))
    lc("openai/gpt-4.1", "hello", logger = logger)
    expect_true(any(grepl("openai/gpt-4.1", logs)))
    expect_true(any(grepl("hello", logs)))
    expect_true(any(grepl("ok", logs)))
  })
})
