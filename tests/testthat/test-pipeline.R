library(testthat)
library(data.table)

# Helper: path to bundled test fixtures
yml_path <- system.file("extdata", "variable_map.yml", package = "cuci")
csv_path <- system.file("extdata", "test-data.csv",    package = "cuci")

# ============================================================
# load_config
# ============================================================

test_that("load_config returns all expected list elements", {
  cfg <- load_config(yml_path)
  expect_named(cfg, c("name_lookup", "label_lookup", "keyword_patterns",
                       "recode_map", "value_map", "missing_map", "type_map", "var_map"))
})

test_that("load_config name_lookup maps aliases to canonicals", {
  cfg <- load_config(yml_path)
  expect_equal(cfg$name_lookup[["Kjonn"]], "kjonn")
  expect_equal(cfg$name_lookup[["Alder"]], "alder")
})

test_that("load_config type_map is character vector", {
  cfg <- load_config(yml_path)
  expect_type(cfg$type_map, "character")
  expect_equal(cfg$type_map[["kjonn"]], "integer")
})

test_that("load_config value_map has correct structure", {
  cfg <- load_config(yml_path)
  vm  <- cfg$value_map[["kjonn"]]
  expect_s3_class(vm, "data.table")
  expect_named(vm, c("num_value", "chr_value"))
  expect_true(1L %in% vm$num_value)
})

test_that("load_config missing_map extracts numeric codes", {
  cfg <- load_config(yml_path)
  # yrkesstatus has missing: [8, 9]
  expect_true(8L %in% cfg$missing_map[["yrkesstatus"]] ||
              "8" %in% as.character(cfg$missing_map[["yrkesstatus"]]))
})

test_that("load_config errors on missing file", {
  expect_error(load_config("nonexistent.yml"), "not found")
})

# ============================================================
# build_keyword_patterns
# ============================================================

test_that("build_keyword_patterns returns named list of regex strings", {
  cfg      <- load_config(yml_path)
  patterns <- build_keyword_patterns(cfg)
  expect_type(patterns, "list")
  expect_true(all(nchar(unlist(patterns)) > 0))
})

test_that("build_keyword_patterns skips variables with no keywords", {
  cfg      <- load_config(yml_path)
  patterns <- build_keyword_patterns(cfg)
  # All returned patterns should be non-null strings
  expect_true(all(vapply(patterns, is.character, logical(1))))
})

# ============================================================
# match_columns
# ============================================================

test_that("match_columns exact-matches normalised column names", {
  cfg    <- load_config(yml_path)
  result <- match_columns(c("kjonn", "alder"), cfg)
  expect_true(nrow(result$match_log) == 2)
  expect_true(all(result$match_log$method == "exact"))
})

test_that("match_columns records unmatched columns", {
  cfg    <- load_config(yml_path)
  result <- match_columns(c("kjonn", "totally_unknown_xyz"), cfg)
  expect_true("totally_unknown_xyz" %in% result$unmatched)
})

test_that("match_columns does not assign the same canonical twice", {
  cfg    <- load_config(yml_path)
  # Two columns that could both match 'kjonn'
  result <- match_columns(c("kjonn", "kjonn2"), cfg)
  canonicals <- result$match_log$canonical
  expect_equal(length(canonicals), length(unique(canonicals)))
})

# ============================================================
# clean_dataset
# ============================================================

test_that("clean_dataset returns list with data and issues", {
  cfg <- load_config(yml_path)
  raw <- data.table::fread(csv_path)
  out <- clean_dataset(raw, cfg, dataset_label = "test")
  expect_named(out, c("data", "issues"))
  expect_s3_class(out$data, "data.table")
  expect_s3_class(out$issues, "data.table")
})

test_that("clean_dataset output only contains canonical columns", {
  cfg         <- load_config(yml_path)
  raw         <- data.table::fread(csv_path)
  out         <- clean_dataset(raw, cfg, dataset_label = "test")
  canonical   <- names(cfg$var_map)
  extra_cols  <- setdiff(names(out$data), c(canonical, "year"))
  expect_length(extra_cols, 0)
})

test_that("clean_dataset issues table has expected columns", {
  cfg <- load_config(yml_path)
  raw <- data.table::fread(csv_path)
  out <- clean_dataset(raw, cfg, dataset_label = "test")
  expect_named(out$issues, c("variable", "issue_type", "detail"))
})

test_that("clean_dataset coercion failure is recorded, not silently applied", {
  cfg <- load_config(yml_path)
  raw <- data.table::fread(csv_path)
  # Introduce a non-integer value into a column declared as integer
  raw_bad       <- data.table::copy(raw)
  raw_bad$Alder <- as.character(raw_bad$Alder)
  raw_bad$Alder[1] <- "not_a_number"
  out <- suppressWarnings(clean_dataset(raw_bad, cfg, dataset_label = "coerce_test"))
  # Should have a coercion_failure entry for 'alder'
  expect_true(any(out$issues$issue_type == "coercion_failure" &
                  out$issues$variable   == "alder"))
})

test_that("clean_dataset unexpected values are flagged in issues", {
  cfg <- load_config(yml_path)
  raw <- data.table::fread(csv_path)
  raw_bad         <- data.table::copy(raw)
  # Inject a value (99) that is not in kjonn's value list [1, 2]
  raw_bad$IO_Kjonn[1] <- 99L
  out <- suppressWarnings(clean_dataset(raw_bad, cfg, dataset_label = "value_test"))
  expect_true(any(out$issues$issue_type == "unexpected_values" &
                  out$issues$variable   == "kjonn"))
})

test_that("clean_dataset injects year column when year_tag is provided", {
  cfg <- load_config(yml_path)
  raw <- data.table::fread(csv_path)
  out <- clean_dataset(raw, cfg, year_tag = 2023L, dataset_label = "test")
  expect_true("year" %in% names(out$data))
  expect_true(all(out$data$year == 2023L))
})

# ============================================================
# validate_dataset
# ============================================================

test_that("validate_dataset returns dt invisibly", {
  cfg <- load_config(yml_path)
  raw <- data.table::fread(csv_path)
  out <- clean_dataset(raw, cfg, dataset_label = "test")
  ret <- validate_dataset(out$data, cfg, "test")
  expect_identical(ret, out$data)
})
