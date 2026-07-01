library(testthat)
library(data.table)

yml_path <- system.file("extdata", "variable_map.yml", package = "cuci")
csv_path <- system.file("extdata", "test-data.csv",    package = "cuci")

# ============================================================
# load_config
# ============================================================

test_that("load_config returns all expected list elements", {
  cfg <- load_config(yml_path)
  expect_named(cfg, c("name_lookup","label_lookup","keyword_patterns",
                       "recode_map","value_map","missing_map","type_map","var_map"))
})

test_that("load_config name_lookup includes normalised alias forms", {
  cfg <- load_config(yml_path)
  # Both raw ("Kjonn") and normalised ("kjonn") should be present
  expect_true("Kjonn" %in% names(cfg$name_lookup))
  expect_true("kjonn" %in% names(cfg$name_lookup))
  expect_equal(cfg$name_lookup[["kjonn"]], "kjonn")
})

test_that("load_config type_map is a named character vector", {
  cfg <- load_config(yml_path)
  expect_type(cfg$type_map, "character")
  expect_equal(cfg$type_map[["kjonn"]], "integer")
})

test_that("load_config value_map has correct structure", {
  cfg <- load_config(yml_path)
  vm  <- cfg$value_map[["kjonn"]]
  expect_s3_class(vm, "data.table")
  expect_named(vm, c("num_value", "chr_value"))
  expect_true(1L %in% vm$num_value && 2L %in% vm$num_value)
})

test_that("load_config recode_map has raw_value and new_value columns", {
  cfg <- load_config(yml_path)
  rc  <- cfg$recode_map[["kjonn"]]
  expect_s3_class(rc, "data.table")
  expect_named(rc, c("raw_value", "new_value"))
})

test_that("load_config missing_map extracts numeric codes", {
  cfg   <- load_config(yml_path)
  codes <- as.integer(cfg$missing_map[["yrkesstatus"]])
  expect_true(8L %in% codes && 9L %in% codes)
})

test_that("load_config errors on missing file", {
  expect_error(load_config("nonexistent.yml"), "not found")
})


# ============================================================
# build_keyword_patterns
# ============================================================

test_that("build_keyword_patterns returns a named list of strings", {
  cfg <- load_config(yml_path)
  p   <- build_keyword_patterns(cfg)
  expect_type(p, "list")
  expect_true(all(vapply(p, is.character, logical(1))))
})

test_that("build_keyword_patterns pattern matches expected column forms", {
  cfg <- load_config(yml_path)
  p   <- cfg$keyword_patterns[["kjonn"]]
  expect_true(grepl(p, "io_kjonn",   perl = TRUE, ignore.case = TRUE))
  expect_true(grepl(p, "kjonn",      perl = TRUE, ignore.case = TRUE))
  expect_false(grepl(p, "kjonnstudie", perl = TRUE, ignore.case = TRUE))
})


# ============================================================
# .build_match_row
# ============================================================

test_that(".build_match_row returns a one-row data.table with correct schema", {
  row <- cuci:::.build_match_row("raw_col", "canonical_col",
                                         "exact", "high", FALSE)
  expect_s3_class(row, "data.table")
  expect_equal(nrow(row), 1L)
  expect_named(row, c("raw_name","canonical","method","confidence","needs_review"))
})


# ============================================================
# .match_exact
# ============================================================

test_that(".match_exact matches known aliases", {
  cfg   <- load_config(yml_path)
  result <- cuci:::.match_exact(
    c("alder", "yrkesstatus"), cfg$name_lookup, character(0)
  )
  matched <- vapply(result$rows, `[[`, character(1), "raw_name")
  expect_true("alder" %in% matched)
  expect_true("yrkesstatus" %in% matched)
  expect_length(result$remaining, 0L)
})

test_that(".match_exact does not claim the same canonical twice", {
  cfg    <- load_config(yml_path)
  # Simulate kjonn already claimed
  result <- cuci:::.match_exact(
    "kjonn", cfg$name_lookup, "kjonn"
  )
  expect_length(result$rows, 0L)
  expect_equal(result$remaining, "kjonn")
})

test_that(".match_exact puts unknown columns in remaining", {
  cfg    <- load_config(yml_path)
  result <- cuci:::.match_exact(
    c("alder", "xyz_unknown"), cfg$name_lookup, character(0)
  )
  expect_equal(result$remaining, "xyz_unknown")
})


# ============================================================
# .match_fuzzy
# ============================================================

test_that(".match_fuzzy catches near-typo column names", {
  cfg    <- load_config(yml_path)
  # "alderr" is one edit away from "alder"
  result <- cuci:::.match_fuzzy(
    "alderr", cfg$name_lookup, character(0)
  )
  expect_length(result$rows, 1L)
  expect_equal(result$rows[[1]]$canonical, "alder")
  expect_equal(result$rows[[1]]$method, "fuzzy")
})

test_that(".match_fuzzy returns nothing for a completely unknown name", {
  cfg    <- load_config(yml_path)
  result <- cuci:::.match_fuzzy(
    "xyz_totally_unknown_abc", cfg$name_lookup, character(0)
  )
  expect_length(result$rows, 0L)
})


# ============================================================
# .find_matched_keyword
# ============================================================

test_that(".find_matched_keyword returns the triggering keyword", {
  cfg <- load_config(yml_path)
  kw  <- cuci:::.find_matched_keyword("io_kjonn", "kjonn", cfg$var_map)
  expect_equal(kw, "kjonn")
})

test_that(".find_matched_keyword returns '(pattern)' when no keyword hits", {
  # Provide a var_map with a keyword that does NOT appear in the column name
  fake_var_map <- list(testvar = list(keywords = list("zzznomatch")))
  kw <- cuci:::.find_matched_keyword("io_kjonn", "testvar", fake_var_map)
  expect_equal(kw, "(pattern)")
})


# ============================================================
# .match_keyword
# ============================================================

test_that(".match_keyword maps io_kjonn -> kjonn via keyword", {
  cfg    <- load_config(yml_path)
  result <- cuci:::.match_keyword(
    "io_kjonn", cfg$keyword_patterns, character(0), cfg$var_map
  )
  expect_length(result$rows, 1L)
  expect_equal(result$rows[[1]]$canonical,  "kjonn")
  expect_equal(result$rows[[1]]$confidence, "low")
  expect_true(grepl("^keyword", result$rows[[1]]$method))
})

test_that(".match_keyword skips already-claimed canonicals", {
  cfg    <- load_config(yml_path)
  result <- cuci:::.match_keyword(
    "io_kjonn", cfg$keyword_patterns, "kjonn", cfg$var_map
  )
  expect_length(result$rows, 0L)
})


# ============================================================
# .compile_match_result
# ============================================================

test_that(".compile_match_result builds rename_vec only for changed names", {
  row1 <- cuci:::.build_match_row("alder",    "alder",    "exact", "high", FALSE)
  row2 <- cuci:::.build_match_row("io_kjonn", "kjonn",    "keyword [\"kjonn\"]", "low", TRUE)
  res  <- cuci:::.compile_match_result(list(row1, row2), c("alder","io_kjonn"))
  # "alder" -> "alder" should NOT be in rename_vec (no change)
  expect_false("alder" %in% names(res$rename_vec))
  # "io_kjonn" -> "kjonn" SHOULD be in rename_vec
  expect_equal(res$rename_vec[["io_kjonn"]], "kjonn")
})

test_that(".compile_match_result returns unmatched correctly", {
  row1 <- cuci:::.build_match_row("alder", "alder", "exact", "high", FALSE)
  res  <- cuci:::.compile_match_result(list(row1), c("alder", "mystery_col"))
  expect_equal(res$unmatched, "mystery_col")
})


# ============================================================
# match_columns  (orchestrator)
# ============================================================

test_that("match_columns returns correct structure", {
  cfg    <- load_config(yml_path)
  result <- match_columns(c("alder", "io_kjonn", "unknown"), cfg)
  expect_named(result, c("match_log", "rename_vec", "unmatched"))
  expect_s3_class(result$match_log, "data.table")
})

test_that("match_columns does not assign the same canonical twice", {
  cfg       <- load_config(yml_path)
  result    <- match_columns(c("kjonn", "io_kjonn"), cfg)
  canonicals <- result$match_log$canonical
  expect_equal(length(canonicals), length(unique(canonicals)))
})

test_that("match_columns puts truly unknown columns in unmatched", {
  cfg    <- load_config(yml_path)
  result <- match_columns(c("alder", "xyzzy_unknown_999"), cfg)
  expect_true("xyzzy_unknown_999" %in% result$unmatched)
})


# ============================================================
# .normalise_colnames
# ============================================================

test_that(".normalise_colnames lowercases and snake_cases names", {
  dt <- data.table(IO_Kjonn = 1L, `My Col!` = 2L)
  cuci:::.normalise_colnames(dt)
  expect_equal(names(dt), c("io_kjonn", "my_col"))
})

test_that(".normalise_colnames deduplicates with suffix", {
  dt <- data.table(A = 1L, a = 2L)
  cuci:::.normalise_colnames(dt)
  expect_true(length(unique(names(dt))) == 2L)
})


# ============================================================
# .apply_renames
# ============================================================

test_that(".apply_renames renames in-place by reference", {
  dt <- data.table(io_kjonn = 1:3)
  cuci:::.apply_renames(dt, c(io_kjonn = "kjonn"))
  expect_true("kjonn" %in% names(dt))
  expect_false("io_kjonn" %in% names(dt))
})

test_that(".apply_renames silently skips names not in dt", {
  dt <- data.table(alder = 1:3)
  expect_silent(cuci:::.apply_renames(dt, c(nonexistent = "kjonn")))
  expect_equal(names(dt), "alder")
})


# ============================================================
# .select_canonical_cols
# ============================================================

test_that(".select_canonical_cols drops non-canonical columns", {
  cfg <- load_config(yml_path)
  dt  <- data.table(kjonn = 1L, alder = 2L, extra_col = 3L)
  out <- cuci:::.select_canonical_cols(dt, cfg)
  expect_false("extra_col" %in% names(out))
  expect_true("kjonn" %in% names(out))
})


# ============================================================
# .apply_recodes
# ============================================================

test_that(".apply_recodes remaps values correctly", {
  cfg <- load_config(yml_path)
  dt  <- data.table(kjonn = c("0","1","2"))
  cuci:::.apply_recodes(dt, cfg, "kjonn")
  expect_equal(dt$kjonn, c(2L, 1L, 2L))
})

test_that(".apply_recodes is a no-op for variables without recode_map", {
  cfg <- load_config(yml_path)
  dt  <- data.table(alder = c(30L, 40L, 50L))
  original <- copy(dt$alder)
  cuci:::.apply_recodes(dt, cfg, "alder")
  expect_equal(dt$alder, original)
})


# ============================================================
# .apply_missing_codes
# ============================================================

test_that(".apply_missing_codes replaces declared codes with NA", {
  cfg <- load_config(yml_path)
  dt  <- data.table(yrkesstatus = c(1L, 8L, 9L, 2L))
  cuci:::.apply_missing_codes(dt, cfg, "yrkesstatus")
  expect_true(is.na(dt$yrkesstatus[2]))
  expect_true(is.na(dt$yrkesstatus[3]))
  expect_false(is.na(dt$yrkesstatus[1]))
})


# ============================================================
# .coerce_column
# ============================================================

test_that(".coerce_column succeeds for clean integer conversion", {
  result <- cuci:::.coerce_column(c("1","2","3"), "integer", "x")
  expect_true(result$success)
  expect_equal(result$converted, c(1L,2L,3L))
  expect_null(result$issue)
})

test_that(".coerce_column fails safely when NAs would be introduced", {
  result <- suppressWarnings(
    cuci:::.coerce_column(c("1","D","3"), "integer", "siv", "test")
  )
  expect_false(result$success)
  expect_null(result$converted)
  expect_equal(result$issue$issue_type, "coercion_failure")
})

test_that(".coerce_column records coercion_skip for unknown target type", {
  result <- cuci:::.coerce_column(1:3, "datestamp", "x")
  expect_false(result$success)
  expect_equal(result$issue$issue_type, "coercion_skip")
})


# ============================================================
# .coerce_all_columns
# ============================================================

test_that(".coerce_all_columns applies safe conversions in-place", {
  cfg <- load_config(yml_path)
  dt  <- data.table(alder = c("30","40","50"))
  cuci:::.coerce_all_columns(dt, cfg, "alder")
  expect_type(dt$alder, "integer")
})

test_that(".coerce_all_columns returns issue for failed column", {
  cfg    <- load_config(yml_path)
  dt     <- data.table(siv = c("1","2","D"))
  issues <- suppressWarnings(
    cuci:::.coerce_all_columns(dt, cfg, "siv", "test")
  )
  expect_length(issues, 1L)
  expect_equal(issues[[1]]$issue_type, "coercion_failure")
  # Column should be unchanged (still character)
  expect_type(dt$siv, "character")
})


# ============================================================
# .validate_column_values
# ============================================================

test_that(".validate_column_values returns NULL when all values valid", {
  result <- cuci:::.validate_column_values(c(1L,2L,1L), c(1L,2L), "kjonn")
  expect_null(result)
})

test_that(".validate_column_values returns issue for unexpected values", {
  result <- suppressWarnings(
    cuci:::.validate_column_values(c(1L,2L,9L), c(1L,2L), "tob1", "test")
  )
  expect_equal(result$issue_type, "unexpected_values")
  expect_true(grepl("9", result$detail))
})

test_that(".validate_column_values ignores NA cells", {
  result <- cuci:::.validate_column_values(c(1L,NA,2L), c(1L,2L), "kjonn")
  expect_null(result)
})


# ============================================================
# .validate_all_values
# ============================================================

test_that(".validate_all_values collects issues across columns", {
  cfg    <- load_config(yml_path)
  dt     <- data.table(kjonn = c(1L,2L,99L), tob1 = c(1L,2L,8L))
  issues <- suppressWarnings(
    cuci:::.validate_all_values(dt, cfg, c("kjonn","tob1"), "test")
  )
  vars <- vapply(issues, function(i) i$variable, character(1))
  expect_true("kjonn" %in% vars)
  expect_true("tob1"  %in% vars)
})


# ============================================================
# .inject_year
# ============================================================

test_that(".inject_year adds integer year column", {
  dt <- data.table(alder = 1:3)
  cuci:::.inject_year(dt, 2023L)
  expect_true("year" %in% names(dt))
  expect_equal(dt$year, rep(2023L, 3))
})

test_that(".inject_year is a no-op when year_tag is NULL", {
  dt <- data.table(alder = 1:3)
  cuci:::.inject_year(dt, NULL)
  expect_false("year" %in% names(dt))
})


# ============================================================
# .drop_empty_rows
# ============================================================

test_that(".drop_empty_rows removes fully-NA rows", {
  dt  <- data.table(a = c(1L, NA, 3L), b = c(2L, NA, 4L))
  out <- cuci:::.drop_empty_rows(dt)
  expect_equal(nrow(out), 2L)
})

test_that(".drop_empty_rows removes exact duplicate rows", {
  dt  <- data.table(a = c(1L,1L,2L), b = c(3L,3L,4L))
  out <- cuci:::.drop_empty_rows(dt)
  expect_equal(nrow(out), 2L)
})


# ============================================================
# .compile_issues
# ============================================================

test_that(".compile_issues returns empty data.table when no issues", {
  out <- cuci:::.compile_issues(list(), list())
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("variable","issue_type","detail"))
})

test_that(".compile_issues stacks issues from multiple sources", {
  i1 <- data.table(variable="a", issue_type="coercion_failure", detail="x")
  i2 <- data.table(variable="b", issue_type="unexpected_values", detail="y")
  out <- cuci:::.compile_issues(list(i1), list(i2))
  expect_equal(nrow(out), 2L)
})


# ============================================================
# clean_dataset  (orchestrator)
# ============================================================

test_that("clean_dataset returns list(data, issues)", {
  cfg <- load_config(yml_path)
  raw <- fread(csv_path)
  out <- suppressWarnings(clean_dataset(raw, cfg, dataset_label = "test"))
  expect_named(out, c("data","issues"))
  expect_s3_class(out$data,   "data.table")
  expect_s3_class(out$issues, "data.table")
})

test_that("clean_dataset output contains only canonical columns (plus year)", {
  cfg <- load_config(yml_path)
  raw <- fread(csv_path)
  out <- suppressWarnings(clean_dataset(raw, cfg, year_tag = 2023L))
  extra <- setdiff(names(out$data), c(names(cfg$var_map), "year"))
  expect_length(extra, 0L)
})

test_that("clean_dataset issues table has correct column names", {
  cfg <- load_config(yml_path)
  raw <- fread(csv_path)
  out <- suppressWarnings(clean_dataset(raw, cfg))
  expect_named(out$issues, c("variable","issue_type","detail"))
})

test_that("clean_dataset records coercion_failure without modifying column type", {
  cfg <- load_config(yml_path)
  raw <- fread(csv_path)
  out <- suppressWarnings(clean_dataset(raw, cfg, dataset_label = "test"))
  expect_true(any(out$issues$issue_type == "coercion_failure"))
  # siv column kept as character because of "D" value
  expect_type(out$data$siv, "character")
})

test_that("clean_dataset injects year column", {
  cfg <- load_config(yml_path)
  raw <- fread(csv_path)
  out <- suppressWarnings(clean_dataset(raw, cfg, year_tag = 2024L))
  expect_true("year" %in% names(out$data))
  expect_true(all(out$data$year == 2024L))
})
