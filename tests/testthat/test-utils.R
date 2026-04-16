describe("make_logger", {
  ml <- grab("make_logger")

  it("returns a no-op when disabled", {
    logger <- ml(FALSE)
    expect_silent(logger("should be silent"))
  })

  it("emits timestamped messages when enabled", {
    logger <- ml(TRUE)
    expect_message(logger("hello"), "\\d{2}:\\d{2}:\\d{2} \\| hello")
  })

  it("concatenates multiple arguments", {
    logger <- ml(TRUE)
    expect_message(logger("a", "b", "c"), "a b c")
  })

  it("appends to reactive sink when provided", {
    rv <- shiny::reactiveValues(log = character())
    logger <- ml(TRUE, sink = rv)
    expect_message(logger("test msg"))
    shiny::isolate({
      expect_length(rv$log, 1)
      expect_match(rv$log[1], "test msg")
    })
  })

  it("caps the sink at max_lines and keeps head and tail", {
    rv <- shiny::reactiveValues(log = character())
    logger <- ml(TRUE, sink = rv, max_lines = 10L)
    for (i in seq_len(15)) {
      expect_message(logger(paste("msg", i)))
    }
    shiny::isolate({
      expect_true(length(rv$log) <= 10)
      expect_true(any(grepl("log trimmed", rv$log)))
      expect_match(rv$log[1], "msg 1")
      expect_match(rv$log[length(rv$log)], "msg 15")
    })
  })

  it("works without sink (CLI mode)", {
    logger <- ml(TRUE)
    expect_message(logger("cli mode"), "cli mode")
  })
})

describe("validate_model_string", {
  vms <- grab("validate_model_string")

  it("accepts valid provider/model format", {
    expect_invisible(vms("openai/gpt-4.1"))
    expect_invisible(vms("anthropic/claude-sonnet-4-5-20250929"))
  })

  it("rejects empty or non-string input", {
    expect_error(vms(""), "non-empty")
    expect_error(vms(NULL), "non-empty")
    expect_error(vms(NA_character_), "non-empty")
    expect_error(vms(42), "non-empty")
  })

  it("rejects model string without slash", {
    expect_error(vms("gpt-4.1"), "provider/model")
    expect_error(vms("openai"), "provider/model")
  })
})

describe("format_context", {
  fc <- grab("format_context")

  it("returns empty string for NULL", {
    expect_equal(fc(NULL), "")
  })

  it("returns empty string for empty string", {
    expect_equal(fc(""), "")
  })

  it("formats non-empty context", {
    expect_equal(fc("PHQ-9"), "\n\nInstrument context: PHQ-9")
  })
})

describe("format_register", {
  fr <- grab("format_register")

  it("returns empty string for NULL", {
    expect_equal(fr(NULL), "")
  })

  it("returns empty string for empty string", {
    expect_equal(fr(""), "")
  })

  it("formats non-empty register", {
    expect_equal(fr("formal"), "\n\nTarget register: formal")
  })
})
