testthat::test_that("sum_up returns counts and percentages", {

  dt <- data.table::data.table(
    kjonn = c(1, 2, 1, 2, 1)
  )

  cfg <- list(
    value_map = list(
      kjonn = data.table::data.table(
        num_value = c(1, 2),
        chr_value = c("Mann", "Kvinne")
      )
    )
  )

  res <- sum_up(dt, kjonn, cfg)

  testthat::expect_s3_class(res, "data.table")

  testthat::expect_equal(
    res$n,
    c(3L, 2L)
  )

  testthat::expect_equal(
    res$pct,
    c(60, 40)
  )
})

testthat::test_that("sum_up applies labels when category = TRUE", {

  dt <- data.table::data.table(
    kjonn = c(1, 2, 1, 2, 1)
  )

  cfg <- list(
    value_map = list(
      kjonn = data.table::data.table(
        num_value = c(1, 2),
        chr_value = c("Mann", "Kvinne")
      )
    )
  )

  res <- sum_up(
    dt,
    kjonn,
    cfg,
    category = TRUE
  )

  testthat::expect_true(is.factor(res$kjonn))

  testthat::expect_equal(
    levels(res$kjonn),
    c("Mann", "Kvinne")
  )

  testthat::expect_equal(
    as.character(res$kjonn),
    c("Mann", "Kvinne")
  )
})

testthat::test_that("sum_up handles haven_labelled variables", {

  dt <- data.table::data.table(
    kjonn = haven::labelled(
      c(1, 2, 1, 2, 1),
      labels = c(
        Mann = 1,
        Kvinne = 2
      )
    )
  )

  cfg <- list(
    value_map = list(
      kjonn = data.table::data.table(
        num_value = c(1, 2),
        chr_value = c("Mann", "Kvinne")
      )
    )
  )

  res <- sum_up(
    dt,
    kjonn,
    cfg,
    category = TRUE
  )

  testthat::expect_true(is.factor(res$kjonn))

  testthat::expect_equal(
    as.character(res$kjonn),
    c("Mann", "Kvinne")
  )

  testthat::expect_equal(
    res$n,
    c(3L, 2L)
  )
})

testthat::test_that("sum_up accepts data.frame input", {

  dt <- data.frame(
    kjonn = c(1, 2, 1, 2, 1)
  )

  cfg <- list(
    value_map = list(
      kjonn = data.table::data.table(
        num_value = c(1, 2),
        chr_value = c("Mann", "Kvinne")
      )
    )
  )

  res <- sum_up(dt, kjonn, cfg)

  testthat::expect_s3_class(
    res,
    "data.table"
  )

  testthat::expect_equal(
    res$n,
    c(3L, 2L)
  )
})

testthat::test_that("sum_up rounds percentages according to digits", {

  dt <- data.table::data.table(
    grp = c(1, 1, 2)
  )

  cfg <- list(
    value_map = list(
      grp = data.table::data.table(
        num_value = c(1, 2),
        chr_value = c("A", "B")
      )
    )
  )

  res <- sum_up(
    dt,
    grp,
    cfg,
    digits = 1
  )

  testthat::expect_equal(
    res$pct,
    c(66.7, 33.3)
  )
})

testthat::test_that("sum_up errors when value_map is missing", {

  dt <- data.table::data.table(
    kjonn = c(1, 2)
  )

  cfg <- list(
    value_map = list()
  )

  testthat::expect_error(
    sum_up(dt, kjonn, cfg),
    "No value_map found for variable 'kjonn'"
  )
})

testthat::test_that("sum_up errors when dt is not a data.frame or data.table", {

  cfg <- list(
    value_map = list(
      kjonn = data.table::data.table(
        num_value = c(1, 2),
        chr_value = c("Mann", "Kvinne")
      )
    )
  )

  testthat::expect_error(
    sum_up(
      dt = c(1, 2, 3),
      kjonn,
      cfg
    ),
    "`dt` must be a data.frame or data.table."
  )
})
