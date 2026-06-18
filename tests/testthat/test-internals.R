# tests/testthat/test-internals.R

test_that("normalize_model maps aliases correctly", {
  nm <- getFromNamespace("normalize_model", "LLMTranslate")
  expect_equal(nm("4o"), "gpt-4o")
  expect_equal(nm(" 4.1-mini "), "gpt-4.1-mini")

  # GPT-5 aliases
  expect_equal(nm("5"), "gpt-5")
  expect_equal(nm("5-mini"), "gpt-5-mini")
  expect_equal(nm("5mini"), "gpt-5-mini")
  expect_equal(nm("5-nano"), "gpt-5-nano")
  expect_equal(nm("5nano"), "gpt-5-nano")
  expect_equal(nm("gpt5"), "gpt-5")
  expect_equal(nm("gpt5-mini"), "gpt-5-mini")
  expect_equal(nm("gpt5-nano"), "gpt-5-nano")

  # Unknown model: returned trimmed original (case preserved)
  expect_equal(nm("SomethingElse"), "SomethingElse")
})

test_that("get_spec returns a row or errors for unsupported models", {
  gs <- getFromNamespace("get_spec", "LLMTranslate")
  expect_s3_class(gs("gpt-4o-mini"), "data.frame")

  # GPT-5 family present
  expect_s3_class(gs("gpt-5"), "data.frame")
  expect_s3_class(gs("gpt-5-mini"), "data.frame")
  expect_s3_class(gs("gpt-5-nano"), "data.frame")

  expect_error(gs("foobar-model"), "Unsupported model")
})

test_that("resolve_spec returns known rows and infers custom models", {
  rs <- getFromNamespace("resolve_spec", "LLMTranslate")

  # Known model -> the canonical MODEL_SPEC row
  expect_equal(rs("gpt-4o-mini")$provider, "openai")
  expect_equal(rs("claude-opus-4-8")$supports_temp, FALSE)

  # Custom / future models the user might type are inferred by provider
  custom_claude <- rs("claude-opus-5")
  expect_equal(custom_claude$provider, "claude")
  expect_false(custom_claude$supports_temp)  # opus 4.x+ rejects temperature

  custom_sonnet <- rs("claude-sonnet-5")
  expect_equal(custom_sonnet$provider, "claude")
  expect_true(custom_sonnet$supports_temp)

  custom_gpt <- rs("gpt-6-turbo")
  expect_equal(custom_gpt$provider, "openai")
  expect_equal(custom_gpt$type, "chat")

  custom_reasoning <- rs("gpt-5.1")
  expect_equal(custom_reasoning$type, "reasoning")
  expect_false(custom_reasoning$supports_temp)

  custom_gemini <- rs("gemini-3.0-pro")
  expect_equal(custom_gemini$provider, "gemini")

  # Genuinely unknown names still error
  expect_error(rs("foobar-model"), "Unsupported model")
})

test_that("infer_provider maps name conventions to providers", {
  ip <- getFromNamespace("infer_provider", "LLMTranslate")
  expect_equal(ip("gpt-4o"), "openai")
  expect_equal(ip("o3-mini"), "openai")
  expect_equal(ip("gemini-2.5-pro"), "gemini")
  expect_equal(ip("claude-haiku-4-5"), "claude")
  expect_true(is.na(ip("mistral-large")))
})

test_that("MODEL_SPEC has required columns", {
  ms <- getFromNamespace("MODEL_SPEC", "LLMTranslate")
  expect_true(all(c("name", "provider", "type", "supports_temp") %in% names(ms)))
  expect_true(nrow(ms) >= 1)
})

test_that("coalesce_chr returns fallback only when needed", {
  co <- getFromNamespace("coalesce_chr", "LLMTranslate")
  expect_equal(co(NULL, "b"), "b")
  expect_equal(co(character(0), "b"), "b")
  expect_equal(co("", "b"), "b")
  expect_equal(co("a", "b"), "a")
})

test_that("%null% operator works as intended", {
  nullop <- getFromNamespace("%null%", "LLMTranslate")
  expect_equal(nullop(NULL, "x"), "x")
  expect_equal(nullop("y", "x"), "y")
})

test_that("strip_code_fences removes markdown fences", {
  scf <- getFromNamespace("strip_code_fences", "LLMTranslate")
  txt <- "```json\n{\"a\":1}\n```"
  expect_equal(scf(txt), "{\"a\":1}")
  expect_equal(scf("no fences"), "no fences")
})

test_that("strip_AB_prefix removes leading letter choices", {
  sap <- getFromNamespace("strip_AB_prefix", "LLMTranslate")
  expect_equal(sap("A) Hello"), "Hello")
  expect_equal(sap("   B)  World"), "World")
  expect_equal(sap("No prefix"), "No prefix")
})

test_that("parse_recon_output handles JSON correctly", {
  pr <- getFromNamespace("parse_recon_output", "LLMTranslate")
  json_in <- '{"revised":"Hallo","explanation":"Minor tweak"}'
  out <- pr(json_in)
  expect_equal(out$revised, "Hallo")
  expect_match(out$explanation, "Minor")
})

test_that("parse_recon_output falls back when JSON missing", {
  pr <- getFromNamespace("parse_recon_output", "LLMTranslate")
  lines_in <- "Hallo\nMinor tweak"
  out2 <- pr(lines_in)
  expect_equal(out2$revised, "Hallo")
  expect_match(out2$explanation, "Minor")
})
