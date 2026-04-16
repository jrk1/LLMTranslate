describe("strip_code_fences", {
  scf <- grab("strip_code_fences")

  it("removes json code fences", {
    expect_equal(scf("```json\n{\"a\":1}\n```"), "{\"a\":1}")
  })

  it("removes plain code fences", {
    expect_equal(scf("```\nhello\n```"), "hello")
  })

  it("leaves unfenced text unchanged", {
    expect_equal(scf("no fences"), "no fences")
  })

  it("handles fences with language tags", {
    expect_equal(scf("```r\ncode\n```"), "code")
    expect_equal(scf("```python\ncode\n```"), "code")
  })
})

describe("strip_AB_prefix", {
  sap <- grab("strip_AB_prefix")

  it("removes A) prefix", {
    expect_equal(sap("A) Hello"), "Hello")
  })

  it("removes B) prefix with leading whitespace", {
    expect_equal(sap("   B)  World"), "World")
  })

  it("leaves text without prefix unchanged", {
    expect_equal(sap("No prefix"), "No prefix")
  })

  it("removes other letter prefixes", {
    expect_equal(sap("C) Option three"), "Option three")
    expect_equal(sap("Z) Last"), "Last")
  })

  it("does not remove lowercase prefixes", {
    expect_equal(sap("a) lowercase"), "a) lowercase")
  })
})

describe("parse_recon_output", {
  pr <- grab("parse_recon_output")

  it("parses valid JSON with revised and explanation", {
    out <- pr('{"revised":"Hallo","explanation":"Minor tweak"}')
    expect_equal(out$revised, "Hallo")
    expect_equal(out$explanation, "Minor tweak")
  })

  it("parses severity field when present", {
    out <- pr('{"revised":"Hallo","explanation":"Changed","severity":"major"}')
    expect_equal(out$severity, "major")
  })

  it("returns NA severity when not in JSON", {
    out <- pr('{"revised":"Hallo","explanation":"OK"}')
    expect_true(is.na(out$severity))
  })

  it("normalizes empty string severity to NA", {
    out <- pr('{"revised":"Hallo","explanation":"OK","severity":""}')
    expect_true(is.na(out$severity))
  })

  it("parses JSON wrapped in code fences", {
    out <- pr('```json\n{"revised":"Hallo","explanation":"Changed"}\n```')
    expect_equal(out$revised, "Hallo")
    expect_equal(out$explanation, "Changed")
  })

  it("falls back to line parsing when JSON is invalid", {
    out <- pr("Hallo\nMinor tweak")
    expect_equal(out$revised, "Hallo")
    expect_match(out$explanation, "Minor")
    expect_true(is.na(out$severity))
  })

  it("falls back when JSON is missing required keys", {
    out <- pr('{"other":"value"}')
    expect_type(out$revised, "character")
  })

  it("handles single-line fallback with no explanation", {
    out <- pr("Just the revised text")
    expect_equal(out$revised, "Just the revised text")
    expect_equal(out$explanation, "")
  })

  it("strips A/B prefixes in fallback mode", {
    out <- pr("A) Revised line\nB) Explanation line")
    expect_equal(out$revised, "Revised line")
    expect_match(out$explanation, "Explanation line")
  })

  it("handles empty input", {
    out <- pr("")
    expect_type(out$revised, "character")
    expect_type(out$explanation, "character")
  })
})

describe("parse_batch_response", {
  pbr <- grab("parse_batch_response")

  it("parses valid JSON array with translations", {
    json <- '[{"item_number":1,"translation":"Hallo"},{"item_number":2,"translation":"Welt"}]'
    out <- pbr(json, "translation", 2)
    expect_equal(out[[1]], "Hallo")
    expect_equal(out[[2]], "Welt")
  })

  it("parses JSON without item_number (uses array order)", {
    json <- '[{"translation":"Hallo"},{"translation":"Welt"}]'
    out <- pbr(json, "translation", 2)
    expect_equal(out[[1]], "Hallo")
    expect_equal(out[[2]], "Welt")
  })

  it("maps by item_number even if out of order", {
    json <- '[{"item_number":2,"translation":"Welt"},{"item_number":1,"translation":"Hallo"}]'
    out <- pbr(json, "translation", 2)
    expect_equal(out[[1]], "Hallo")
    expect_equal(out[[2]], "Welt")
  })

  it("fills missing items with error message", {
    json <- '[{"item_number":1,"translation":"Hallo"}]'
    out <- pbr(json, "translation", 3)
    expect_equal(out[[1]], "Hallo")
    expect_match(out[[2]], "ERROR")
    expect_match(out[[3]], "ERROR")
  })

  it("handles missing field in JSON objects", {
    json <- '[{"item_number":1,"other":"value"}]'
    out <- pbr(json, "translation", 1)
    expect_match(out[[1]], "ERROR: Missing field")
  })

  it("warns on out-of-range item_number", {
    json <- '[{"item_number":99,"translation":"Hallo"}]'
    logs <- character()
    logger <- function(...) logs <<- c(logs, paste(..., collapse = " "))
    out <- pbr(json, "translation", 2, logger)
    expect_match(out[[1]], "ERROR")
    expect_match(out[[2]], "ERROR")
    expect_true(any(grepl("out of range", logs)))
  })

  it("falls back to line-by-line when JSON is invalid", {
    text <- "1. Hallo\n2. Welt"
    out <- pbr(text, "translation", 2)
    expect_equal(out[[1]], "Hallo")
    expect_equal(out[[2]], "Welt")
  })

  it("line-by-line fallback strips leading numbers", {
    text <- "1. First\n2. Second\n3. Third"
    out <- pbr(text, "translation", 3)
    expect_equal(out[[1]], "First")
    expect_equal(out[[2]], "Second")
    expect_equal(out[[3]], "Third")
  })

  it("line-by-line fallback preserves items starting with digits", {
    text <- "3 times per week\n5 days a month"
    out <- pbr(text, "translation", 2)
    expect_equal(out[[1]], "3 times per week")
    expect_equal(out[[2]], "5 days a month")
  })

  it("line-by-line fallback fills short responses with errors", {
    text <- "Only one line"
    out <- pbr(text, "translation", 3)
    expect_equal(out[[1]], "Only one line")
    expect_match(out[[2]], "ERROR")
    expect_match(out[[3]], "ERROR")
  })

  it("handles code-fenced JSON", {
    json <- '```json\n[{"translation":"Hallo"}]\n```'
    out <- pbr(json, "translation", 1)
    expect_equal(out[[1]], "Hallo")
  })

  it("handles back_translation field name", {
    json <- '[{"back_translation":"Hello"}]'
    out <- pbr(json, "back_translation", 1)
    expect_equal(out[[1]], "Hello")
  })

  it("handles more JSON items than n_items", {
    json <- '[{"translation":"A"},{"translation":"B"},{"translation":"C"}]'
    out <- pbr(json, "translation", 2)
    expect_length(out, 2)
    expect_equal(out[[1]], "A")
    expect_equal(out[[2]], "B")
  })

  it("handles duplicate item_numbers by last-write-wins", {
    json <- '[{"item_number":1,"translation":"First"},{"item_number":1,"translation":"Second"}]'
    out <- pbr(json, "translation", 1)
    expect_equal(out[[1]], "Second")
  })

  it("handles empty lines in line-by-line fallback", {
    text <- "First\n\n\nSecond"
    out <- pbr(text, "translation", 2)
    expect_equal(out[[1]], "First")
    expect_equal(out[[2]], "Second")
  })
})

describe("parse_batch_recon_response", {
  pbrr <- grab("parse_batch_recon_response")

  it("parses valid JSON array with revised and explanation", {
    json <- '[{"item_number":1,"revised":"Hallo","explanation":"OK"},{"item_number":2,"revised":"Welt","explanation":"Changed"}]'
    out <- pbrr(json, 2)
    expect_equal(out[[1]]$revised, "Hallo")
    expect_equal(out[[1]]$explanation, "OK")
    expect_equal(out[[2]]$revised, "Welt")
    expect_equal(out[[2]]$explanation, "Changed")
  })

  it("extracts severity field when present", {
    json <- '[{"item_number":1,"revised":"Hallo","explanation":"OK","severity":"minor"}]'
    out <- pbrr(json, 1)
    expect_equal(out[[1]]$severity, "minor")
  })

  it("returns NA severity when field is missing", {
    json <- '[{"item_number":1,"revised":"Hallo","explanation":"OK"}]'
    out <- pbrr(json, 1)
    expect_true(is.na(out[[1]]$severity))
  })

  it("normalizes empty string severity to NA", {
    json <- '[{"item_number":1,"revised":"Hallo","explanation":"OK","severity":""}]'
    out <- pbrr(json, 1)
    expect_true(is.na(out[[1]]$severity))
  })

  it("parses JSON without item_number (uses array order)", {
    json <- '[{"revised":"Hallo","explanation":"OK"}]'
    out <- pbrr(json, 1)
    expect_equal(out[[1]]$revised, "Hallo")
  })

  it("maps by item_number even if out of order", {
    json <- '[{"item_number":2,"revised":"B","explanation":""},{"item_number":1,"revised":"A","explanation":""}]'
    out <- pbrr(json, 2)
    expect_equal(out[[1]]$revised, "A")
    expect_equal(out[[2]]$revised, "B")
  })

  it("fills missing items with error", {
    json <- '[{"item_number":1,"revised":"Hallo","explanation":"OK"}]'
    out <- pbrr(json, 3)
    expect_equal(out[[1]]$revised, "Hallo")
    expect_match(out[[2]]$revised, "ERROR")
    expect_match(out[[3]]$revised, "ERROR")
  })

  it("handles missing revised field", {
    json <- '[{"explanation":"only explanation"}]'
    out <- pbrr(json, 1)
    expect_match(out[[1]]$revised, "ERROR: Missing revised field")
    expect_equal(out[[1]]$explanation, "only explanation")
  })

  it("handles missing explanation field", {
    json <- '[{"revised":"Hallo"}]'
    out <- pbrr(json, 1)
    expect_equal(out[[1]]$revised, "Hallo")
    expect_equal(out[[1]]$explanation, "")
  })

  it("warns on out-of-range item_number", {
    json <- '[{"item_number":99,"revised":"Hallo","explanation":""}]'
    logs <- character()
    logger <- function(...) logs <<- c(logs, paste(..., collapse = " "))
    out <- pbrr(json, 1, logger)
    expect_match(out[[1]]$revised, "ERROR")
    expect_true(any(grepl("out of range", logs)))
  })

  it("falls back to error list when JSON is invalid", {
    out <- pbrr("not json at all", 2)
    expect_match(out[[1]]$revised, "ERROR")
    expect_match(out[[2]]$revised, "ERROR")
    expect_equal(out[[1]]$explanation, "")
  })

  it("handles code-fenced JSON", {
    json <- '```json\n[{"revised":"Hallo","explanation":"OK"}]\n```'
    out <- pbrr(json, 1)
    expect_equal(out[[1]]$revised, "Hallo")
  })

  it("handles more JSON items than n_items", {
    json <- '[{"revised":"A","explanation":""},{"revised":"B","explanation":""},{"revised":"C","explanation":""}]'
    out <- pbrr(json, 2)
    expect_length(out, 2)
    expect_equal(out[[1]]$revised, "A")
    expect_equal(out[[2]]$revised, "B")
  })

  it("handles duplicate item_numbers by last-write-wins", {
    json <- '[{"item_number":1,"revised":"First","explanation":""},{"item_number":1,"revised":"Second","explanation":""}]'
    out <- pbrr(json, 1)
    expect_equal(out[[1]]$revised, "Second")
  })
})
