#' Strip 'A)' / 'B)' prefixes often found in LLM outputs
#'
#' @keywords internal
#' @noRd
strip_AB_prefix <- function(x) {
  sub("^\\s*[A-Z]\\)\\s*", "", x, perl = TRUE)
}

#' Remove Markdown code fences from a string
#'
#' @keywords internal
#' @noRd
strip_code_fences <- function(txt) {
  txt <- trimws(txt)
  txt <- sub("^```[a-zA-Z0-9_-]*\\s*", "", txt)
  txt <- sub("```\\s*$", "", txt)
  trimws(txt)
}

#' Map parsed JSON items into a fixed-length result list by item_number
#'
#' @param parsed List of parsed JSON objects (each may have `item_number`).
#' @param values List of extracted values, same length as `parsed`.
#' @param n_items Expected number of items.
#' @param fill_value Value to use for missing slots.
#' @param fill_label Label for log warnings about missing items.
#' @param logger Logger function.
#'
#' @keywords internal
#' @noRd
map_by_item_number <- function(
  parsed,
  values,
  n_items,
  fill_value,
  fill_label = "item",
  logger = function(...) {}
) {
  result <- vector("list", n_items)

  indices <- vapply(
    parsed,
    function(item) {
      num <- item[["item_number"]]
      if (!is.null(num)) as.integer(num) else NA_integer_
    },
    integer(1)
  )

  for (j in seq_along(parsed)) {
    idx <- indices[j]
    if (!is.na(idx)) {
      if (idx >= 1L && idx <= n_items) {
        if (!is.null(result[[idx]])) {
          logger(paste(
            "Warning: Duplicate item_number", idx, "- overwriting previous value"
          ))
        }
        result[[idx]] <- values[[j]]
      } else {
        logger(paste(
          "Warning: Item number", idx, "out of range (1 to", n_items, ")"
        ))
      }
    } else if (j <= n_items) {
      logger(paste("Warning: No item_number for item at position", j, "- using array order"))
      result[[j]] <- values[[j]]
    } else {
      logger(paste("Warning: Extra item at position", j, "dropped (expected", n_items, "items)"))
    }
  }

  missing <- vapply(result, is.null, logical(1))
  if (any(missing)) {
    for (i in which(missing)) {
      logger(paste("Warning: Missing", fill_label, "for item", i))
    }
    result[missing] <- if (is.list(fill_value)) list(fill_value) else fill_value
  }
  result
}

#' Parse reconciliation JSON or fallback to first-line parsing
#'
#' @importFrom jsonlite fromJSON
#' @keywords internal
#' @noRd
parse_recon_output <- function(txt) {
  txt2 <- strip_code_fences(txt)
  try_json <- try(jsonlite::fromJSON(txt2), silent = TRUE)
  if (
    !inherits(try_json, "try-error") &&
      all(c("revised", "explanation") %in% names(try_json))
  ) {
    sev <- try_json$severity %||% NA_character_
    if (!is.na(sev) && !nzchar(sev)) sev <- NA_character_
    return(list(
      revised = try_json$revised,
      explanation = try_json$explanation,
      severity = sev
    ))
  }
  parts <- strsplit(txt2, "\n", fixed = TRUE)[[1]]
  if (!length(parts)) {
    return(list(revised = txt2, explanation = "", severity = NA_character_))
  }
  list(
    revised = strip_AB_prefix(parts[1]),
    explanation = if (length(parts) > 1) {
      strip_AB_prefix(paste(parts[-1], collapse = " "))
    } else {
      ""
    },
    severity = NA_character_
  )
}

#' Parse batch translation response (forward or backward)
#'
#' @keywords internal
#' @noRd
parse_batch_response <- function(
  txt,
  field_name,
  n_items,
  logger = function(...) {}
) {
  txt2 <- strip_code_fences(txt)
  parsed <- try(jsonlite::fromJSON(txt2, simplifyVector = FALSE), silent = TRUE)

  if (inherits(parsed, "try-error") || !is.list(parsed)) {
    logger(
      "Failed to parse batch response as JSON. Attempting line-by-line parsing..."
    )
    lines <- strsplit(txt2, "\n", fixed = TRUE)[[1]]
    lines <- lines[nzchar(trimws(lines))]
    cleaned <- vapply(seq_along(lines), function(j) {
      sub("^\\s*\\d+\\.\\s+", "", lines[j])
    }, character(1))
    result <- as.list(rep("ERROR: Could not parse response", n_items))
    n_fill <- min(n_items, length(cleaned))
    result[seq_len(n_fill)] <- as.list(cleaned[seq_len(n_fill)])
    return(result)
  }

  values <- vapply(
    parsed,
    function(item) {
      as.character(item[[field_name]] %||% "ERROR: Missing field")
    },
    character(1)
  )

  map_by_item_number(
    parsed, as.list(values), n_items,
    fill_value = "ERROR: Missing translation in response",
    fill_label = field_name, logger = logger
  )
}

#' Parse batch reconciliation response
#'
#' @keywords internal
#' @noRd
parse_batch_recon_response <- function(
  txt,
  n_items,
  logger = function(...) {}
) {
  txt2 <- strip_code_fences(txt)
  parsed <- try(jsonlite::fromJSON(txt2, simplifyVector = FALSE), silent = TRUE)

  error_item <- list(
    revised = "ERROR: Could not parse response",
    explanation = "",
    severity = NA_character_
  )

  if (inherits(parsed, "try-error") || !is.list(parsed)) {
    logger(
      "Failed to parse batch reconciliation response as JSON. Using fallback..."
    )
    return(rep(list(error_item), n_items))
  }

  values <- lapply(parsed, function(item) {
    sev <- item[["severity"]] %||% NA_character_
    if (!is.na(sev) && !nzchar(sev)) sev <- NA_character_
    list(
      revised = as.character(
        item[["revised"]] %||% "ERROR: Missing revised field"
      ),
      explanation = as.character(item[["explanation"]] %||% ""),
      severity = as.character(sev)
    )
  })

  map_by_item_number(
    parsed, values, n_items,
    fill_value = list(
      revised = "ERROR: Missing item in response",
      explanation = "",
      severity = NA_character_
    ),
    fill_label = "reconciliation data", logger = logger
  )
}
