describe("batch_forward", {
  bf <- grab("batch_forward")

  it("formats numbered list and calls llm_call once", {
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        '[{"item_number":1,"translation":"Hallo"},{"item_number":2,"translation":"Welt"}]'
      },
      .package = "LLMTranslate"
    )
    result <- bf(
      c("Hello", "World"), "openai/gpt-4.1", "English", "German"
    )
    expect_equal(result, c("Hallo", "Welt"))
  })

  it("passes custom prompt_template", {
    captured_prompt <- NULL
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        captured_prompt <<- prompt
        '[{"item_number":1,"translation":"X"}]'
      },
      .package = "LLMTranslate"
    )
    custom <- "Translate {items_text} from {from_lang} to {to_lang}{context}{register}"
    bf(c("Hello"), "openai/gpt-4.1", "English", "German",
       prompt_template = custom)
    expect_match(captured_prompt, "^Translate 1\\. Hello")
  })
})

describe("batch_back", {
  bb <- grab("batch_back")

  it("parses back_translation field from response", {
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        '[{"item_number":1,"back_translation":"Hello"},{"item_number":2,"back_translation":"World"}]'
      },
      .package = "LLMTranslate"
    )
    result <- bb(
      c("Hallo", "Welt"), "openai/gpt-4.1", "English", "German"
    )
    expect_equal(result, c("Hello", "World"))
  })
})

describe("batch_translate edge cases", {
  bt <- grab("batch_translate")
  pf <- grab("prompt_batch_forward")

  it("returns NA for list or multi-length values in parsed response", {
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        '[{"item_number":1,"translation":["a","b"]},{"item_number":2,"translation":"Welt"}]'
      },
      parse_batch_response = function(response, field_name, n, logger) {
        list(list("a", "b"), "Welt")
      },
      .package = "LLMTranslate"
    )
    result <- bt(
      c("Hello", "World"), "openai/gpt-4.1", "English", "German",
      field_name = "translation", prompt_template = pf
    )
    expect_equal(result, c(NA_character_, "Welt"))
  })
})

describe("batch_reconcile", {
  br <- grab("batch_reconcile")

  it("returns revised, explanation, and severity", {
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        '[{"item_number":1,"revised":"Hallo","severity":"none","explanation":"OK"},{"item_number":2,"revised":"Welt","severity":"major","explanation":"Changed"}]'
      },
      .package = "LLMTranslate"
    )
    result <- br(
      c("Hello", "World"), c("Hallo", "Welt"), c("Hello", "Earth"),
      "openai/gpt-4.1", "English", "German"
    )
    expect_equal(result$revised, c("Hallo", "Welt"))
    expect_equal(result$severity, c("none", "major"))
    expect_equal(result$explanation, c("OK", "Changed"))
  })

  it("formats ORIGINAL/FORWARD/BACK block in prompt", {
    captured_prompt <- NULL
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        captured_prompt <<- prompt
        '[{"item_number":1,"revised":"X","explanation":"Y","severity":"none"}]'
      },
      .package = "LLMTranslate"
    )
    br(
      c("Hello"), c("Hallo"), c("Hello"),
      "openai/gpt-4.1", "English", "German"
    )
    expect_match(captured_prompt, "ORIGINAL: Hello")
    expect_match(captured_prompt, "FORWARD: Hallo")
    expect_match(captured_prompt, "BACK: Hello")
  })
})
