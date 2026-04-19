describe("prompt templates", {
  prompts <- list(
    forward       = grab("prompt_forward"),
    back          = grab("prompt_back"),
    recon         = grab("prompt_recon"),
    batch_forward = grab("prompt_batch_forward"),
    batch_back    = grab("prompt_batch_back"),
    batch_recon   = grab("prompt_batch_recon")
  )

  it("all prompts are non-empty strings", {
    for (nm in names(prompts)) {
      expect_type(prompts[[nm]], "character")
      expect_true(nzchar(prompts[[nm]]), info = nm)
    }
  })

  it("item-level prompts contain required placeholders", {
    expect_match(prompts$forward, "\\{from_lang\\}")
    expect_match(prompts$forward, "\\{to_lang\\}")
    expect_match(prompts$forward, "\\{text\\}")
    expect_match(prompts$forward, "\\{context\\}")
    expect_match(prompts$forward, "\\{register\\}")

    expect_match(prompts$back, "\\{from_lang\\}")
    expect_match(prompts$back, "\\{to_lang\\}")
    expect_match(prompts$back, "\\{text\\}")

    expect_match(prompts$recon, "\\{from_lang\\}")
    expect_match(prompts$recon, "\\{to_lang\\}")
  })

  it("batch prompts contain required placeholders", {
    expect_match(prompts$batch_forward, "\\{items_text\\}")
    expect_match(prompts$batch_forward, "\\{from_lang\\}")
    expect_match(prompts$batch_forward, "\\{to_lang\\}")
    expect_match(prompts$batch_forward, "\\{context\\}")
    expect_match(prompts$batch_forward, "\\{register\\}")

    expect_match(prompts$batch_back, "\\{items_text\\}")
    expect_match(prompts$batch_recon, "\\{items_text\\}")
  })

  it("forward prompts resolve with glue_data without error", {
    result <- glue::glue_data(
      list(text = "I feel happy", from_lang = "English", to_lang = "German",
           context = "", register = ""),
      prompts$forward
    )
    expect_type(as.character(result), "character")
    expect_match(result, "I feel happy")
    expect_match(result, "German")
  })

  it("forward prompts include context when provided", {
    result <- glue::glue_data(
      list(text = "test", from_lang = "English", to_lang = "German",
           context = "\n\nInstrument context: PHQ-9 depression scale",
           register = ""),
      prompts$forward
    )
    expect_match(result, "PHQ-9")
  })

  it("forward prompts include register when provided", {
    result <- glue::glue_data(
      list(text = "test", from_lang = "English", to_lang = "German",
           context = "",
           register = "\n  Target register: formal clinical language"),
      prompts$forward
    )
    expect_match(result, "formal clinical")
  })

  it("back prompt resolves without context/register", {
    result <- glue::glue_data(
      list(text = "Ich bin gluecklich", from_lang = "English", to_lang = "German"),
      prompts$back
    )
    expect_match(result, "gluecklich")
  })

  it("recon prompts contain context and register placeholders", {
    expect_match(prompts$recon, "\\{context\\}")
    expect_match(prompts$recon, "\\{register\\}")
    expect_match(prompts$batch_recon, "\\{context\\}")
    expect_match(prompts$batch_recon, "\\{register\\}")
  })

  it("recon prompt resolves with context and register", {
    result <- glue::glue_data(
      list(from_lang = "English", to_lang = "German",
           context = "\n\nInstrument context: PHQ-9 depression scale",
           register = "\n\nTarget register: formal clinical language"),
      prompts$recon
    )
    expect_match(result, "PHQ-9")
    expect_match(result, "formal clinical")
  })

  it("batch recon prompt resolves with context and register", {
    result <- glue::glue_data(
      list(items_text = "Item 1:\nORIGINAL: test\nFORWARD: test\nBACK: test",
           from_lang = "English", to_lang = "German",
           context = "\n\nInstrument context: PHQ-9",
           register = "\n\nTarget register: informal"),
      prompts$batch_recon
    )
    expect_match(result, "PHQ-9")
    expect_match(result, "informal")
  })

  it("recon prompt mentions severity classification", {
    expect_match(prompts$recon, "severity")
    expect_match(prompts$recon, "none")
    expect_match(prompts$recon, "minor")
    expect_match(prompts$recon, "major")
  })

  it("batch prompts resolve with glue_data without error", {
    result <- glue::glue_data(
      list(items_text = "1. Hello\n2. Goodbye", from_lang = "English",
           to_lang = "German", context = "", register = ""),
      prompts$batch_forward
    )
    expect_match(result, "Hello")
  })

  it("prompts mention TRAPD/ISPOR", {
    for (nm in names(prompts)) {
      expect_match(prompts[[nm]], "TRAPD|ISPOR", info = nm)
    }
  })

  it("prompts mention response scale anchors", {
    expect_match(prompts$forward, "response scale")
    expect_match(prompts$back, "response scale")
  })

  it("batch forward prompt mentions reverse-coded items", {
    expect_match(prompts$batch_forward, "reverse")
  })
})
