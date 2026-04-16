describe("available_providers", {
  ap <- grab("available_providers")

  it("returns a character vector with known providers", {
    providers <- ap()
    expect_type(providers, "character")
    expect_true(length(providers) > 0)
    expect_true("openai" %in% providers)
    expect_true("anthropic" %in% providers)
    expect_true("google_gemini" %in% providers)
  })

  it("excludes _test providers", {
    providers <- ap()
    expect_false(any(grepl("_test$", providers)))
  })
})

describe("available_models", {
  am <- grab("available_models")

  it("returns NULL for nonexistent provider", {
    expect_null(am("nonexistent_provider_xyz"))
  })

  it("returns NULL or character for unconfigured provider", {
    withr::with_envvar(c(OPENAI_API_KEY = ""), {
      result <- am("openai")
      expect_true(is.null(result) || is.character(result))
    })
  })

  it("returns model IDs when API key is configured", {
    skip_if(
      Sys.getenv("OPENAI_API_KEY") == "",
      "OPENAI_API_KEY not set"
    )
    result <- am("openai")
    expect_type(result, "character")
    expect_true(length(result) > 0)
  })
})
