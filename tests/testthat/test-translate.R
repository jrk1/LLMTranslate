describe("translate_batch", {
  it("errors on non-data-frame non-file input", {
    expect_error(translate_batch(42, "col", "EN", "DE"), "data frame")
  })

  it("errors on missing column", {
    df <- data.frame(item = "hello")
    expect_error(translate_batch(df, "nonexistent", "EN", "DE"), "not found")
  })

  it("returns data unchanged when all items are empty", {
    df <- data.frame(item = c("", NA, "  "))
    result <- translate_batch(df, "item", "English", "German",
                              model = "openai/gpt-4.1", back_model = NULL)
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 1)
  })

  it("runs forward-only with mocked LLM", {
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        '[{"item_number":1,"translation":"Hallo"},{"item_number":2,"translation":"Traurig"}]'
      },
      .package = "LLMTranslate"
    )
    df <- data.frame(item = c("I feel happy", "I feel sad"))
    result <- translate_batch(
      df, "item", "English", "German",
      model = "openai/gpt-4.1", back_model = NULL
    )
    expect_true("Forward_German" %in% names(result))
    expect_equal(result$Forward_German, c("Hallo", "Traurig"))
  })

  it("runs full forward -> back -> recon pipeline with mocked LLM", {
    call_count <- 0L
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        call_count <<- call_count + 1L
        switch(
          as.character(call_count),
          "1" = '[{"item_number":1,"translation":"Hallo"},{"item_number":2,"translation":"Traurig"}]',
          "2" = '[{"item_number":1,"back_translation":"Hello"},{"item_number":2,"back_translation":"Sad"}]',
          "3" = '[{"item_number":1,"revised":"Hallo","explanation":"OK","severity":"none"},{"item_number":2,"revised":"Traurig","explanation":"Changed wording","severity":"minor"}]'
        )
      },
      .package = "LLMTranslate"
    )
    df <- data.frame(item = c("I feel happy", "I feel sad"))
    result <- translate_batch(
      df, "item", "English", "German",
      model = "openai/gpt-4.1"
    )
    expect_equal(call_count, 3L)
    expect_true("Forward_German" %in% names(result))
    expect_true("Back_English" %in% names(result))
    expect_true("Reconciled_German" %in% names(result))
    expect_true("Recon_Explanation" %in% names(result))
    expect_true("Recon_Severity" %in% names(result))
    expect_equal(result$Forward_German, c("Hallo", "Traurig"))
    expect_equal(result$Back_English, c("Hello", "Sad"))
    expect_equal(result$Reconciled_German, c("Hallo", "Traurig"))
    expect_equal(result$Recon_Severity, c("none", "minor"))
  })

  it("passes context and register into the LLM prompt", {
    captured_prompt <- NULL
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        captured_prompt <<- prompt
        '[{"item_number":1,"translation":"Hallo"}]'
      },
      .package = "LLMTranslate"
    )
    df <- data.frame(item = "I feel happy")
    translate_batch(
      df, "item", "English", "German",
      model = "openai/gpt-4.1", back_model = NULL,
      context = "PHQ-9 depression screening",
      register = "formal clinical language"
    )
    expect_match(captured_prompt, "PHQ-9")
    expect_match(captured_prompt, "formal clinical")
  })

  it("catches forward translation error and returns ERROR string", {
    local_mocked_bindings(
      batch_forward = function(...) stop("API timeout"),
      .package = "LLMTranslate"
    )
    df <- data.frame(item = c("hello", "world"))
    result <- translate_batch(
      df, "item", "English", "German",
      model = "openai/gpt-4.1", back_model = NULL
    )
    expect_true(all(grepl("^ERROR:", result$Forward_German)))
  })

  it("catches back-translation error and returns ERROR string", {
    call_count <- 0L
    local_mocked_bindings(
      batch_forward = function(...) c("Hallo", "Welt"),
      batch_back = function(...) stop("API timeout"),
      .package = "LLMTranslate"
    )
    df <- data.frame(item = c("hello", "world"))
    result <- translate_batch(
      df, "item", "English", "German",
      model = "openai/gpt-4.1", recon_model = NULL
    )
    expect_true(all(grepl("^ERROR:", result$Back_English)))
  })

  it("catches reconciliation error and returns ERROR list", {
    local_mocked_bindings(
      batch_forward = function(...) c("Hallo", "Welt"),
      batch_back = function(...) c("Hello", "World"),
      batch_reconcile = function(...) stop("API timeout"),
      .package = "LLMTranslate"
    )
    df <- data.frame(item = c("hello", "world"))
    result <- translate_batch(
      df, "item", "English", "German",
      model = "openai/gpt-4.1"
    )
    expect_true(all(grepl("^ERROR:", result$Reconciled_German)))
    expect_true(all(result$Recon_Explanation == ""))
  })

  it("returns after back-translation when recon_model is NULL", {
    local_mocked_bindings(
      batch_forward = function(...) "Hallo",
      batch_back = function(...) "Hello",
      .package = "LLMTranslate"
    )
    df <- data.frame(item = "hello")
    result <- translate_batch(
      df, "item", "English", "German",
      model = "openai/gpt-4.1", recon_model = NULL
    )
    expect_true("Back_English" %in% names(result))
    expect_false("Reconciled_German" %in% names(result))
  })

  it("errors on model string without provider/model format", {
    df <- data.frame(item = "hello")
    expect_error(
      translate_batch(df, "item", "EN", "DE", model = "gpt-4.1"),
      "provider/model"
    )
  })

  it("reads Excel file path with mocked LLM", {
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        '[{"item_number":1,"translation":"Hallo"},{"item_number":2,"translation":"Welt"}]'
      },
      .package = "LLMTranslate"
    )
    tmp <- tempfile(fileext = ".xlsx")
    on.exit(unlink(tmp))
    df <- data.frame(item = c("hello", "world"))
    openxlsx::write.xlsx(df, tmp)

    result <- translate_batch(tmp, "item", "English", "German",
                              model = "openai/gpt-4.1", back_model = NULL)
    expect_true("Forward_German" %in% names(result))
    expect_equal(nrow(result), 2)
    expect_equal(result$Forward_German, c("Hallo", "Welt"))
  })
})

describe("translate_item", {
  it("errors on non-data-frame non-file input", {
    expect_error(translate_item(42, "col", "EN", "DE"), "data frame")
  })

  it("errors on missing column", {
    df <- data.frame(item = "hello")
    expect_error(translate_item(df, "nonexistent", "EN", "DE"), "not found")
  })

  it("returns data unchanged when all items are empty", {
    df <- data.frame(item = c("", NA, "  "))
    result <- translate_item(df, "item", "English", "German",
                             model = "openai/gpt-4.1", back_model = NULL)
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 1)
  })

  it("runs full forward -> back -> recon pipeline with mocked LLM", {
    call_count <- 0L
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        call_count <<- call_count + 1L
        stage <- (call_count - 1L) %% 3L + 1L
        switch(
          as.character(stage),
          "1" = "Hallo",
          "2" = "Hello",
          "3" = '{"revised":"Hallo","explanation":"OK","severity":"none"}'
        )
      },
      .package = "LLMTranslate"
    )
    df <- data.frame(item = c("I feel happy"))
    result <- translate_item(
      df, "item", "English", "German",
      model = "openai/gpt-4.1"
    )
    expect_equal(call_count, 3L)
    expect_true("Forward_German" %in% names(result))
    expect_true("Back_English" %in% names(result))
    expect_true("Reconciled_German" %in% names(result))
    expect_true("Recon_Explanation" %in% names(result))
    expect_true("Recon_Severity" %in% names(result))
    expect_equal(result$Forward_German, "Hallo")
    expect_equal(result$Back_English, "Hello")
    expect_equal(result$Reconciled_German, "Hallo")
    expect_equal(result$Recon_Severity, "none")
  })

  it("returns forward-only when back_model is NULL", {
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) "Hallo",
      .package = "LLMTranslate"
    )
    df <- data.frame(item = "hello")
    result <- translate_item(
      df, "item", "English", "German",
      model = "openai/gpt-4.1", back_model = NULL
    )
    expect_true("Forward_German" %in% names(result))
    expect_false("Back_English" %in% names(result))
  })

  it("returns after back-translation when recon_model is NULL", {
    call_count <- 0L
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        call_count <<- call_count + 1L
        if (call_count == 1L) "Hallo" else "Hello"
      },
      .package = "LLMTranslate"
    )
    df <- data.frame(item = "hello")
    result <- translate_item(
      df, "item", "English", "German",
      model = "openai/gpt-4.1", recon_model = NULL
    )
    expect_true("Back_English" %in% names(result))
    expect_false("Reconciled_German" %in% names(result))
  })

  it("catches reconciliation error per item", {
    call_count <- 0L
    local_mocked_bindings(
      llm_call = function(model, prompt, temperature, logger, ...) {
        call_count <<- call_count + 1L
        switch(
          as.character(call_count),
          "1" = "Hallo",
          "2" = "Hello",
          stop("recon failed")
        )
      },
      .package = "LLMTranslate"
    )
    df <- data.frame(item = "hello")
    result <- translate_item(
      df, "item", "English", "German",
      model = "openai/gpt-4.1"
    )
    expect_match(result$Reconciled_German, "^ERROR:")
    expect_equal(result$Recon_Explanation, "")
  })
})
