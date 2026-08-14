testthat::test_that("extract_attr extracts labels and value labels", {

  dt <- data.table::data.table(
    kjonn = haven::labelled(
      c(1, 2),
      labels = c(Mann = 1, Kvinne = 2)
    ),
    alder = c(20, 30)
  )

  attr(dt$kjonn, "label") <- "Kjønn"
  attr(dt$alder, "label") <- "Alder"

  res <- extract_attr(dt)

  testthat::expect_s3_class(res, "data.table")
  testthat::expect_equal(nrow(res), 2)

  testthat::expect_equal(
    res[name == "kjonn", label],
    "Kjønn"
  )

  testthat::expect_match(
    res[name == "kjonn", value_labels],
    "1 Mann<br>2 Kvinne"
  )

  testthat::expect_equal(
    res[name == "alder", value_labels],
    ""
  )
})


testthat::test_that(
  "make_codebook errors when haven attributes are missing and config is NULL",
  {

    dt <- data.table::data.table(
      x = 1:3,
      y = c("a", "b", "c")
    )

    testthat::expect_error(
      make_codebook(dt),
      "Dataset does not have haven attributes"
    )
  }
)


testthat::test_that(
  "make_codebook returns a datatable object",
  {

    dt <- data.table::data.table(
      kjonn = haven::labelled(
        c(1, 2),
        labels = c(Mann = 1, Kvinne = 2)
      )
    )

    attr(dt$kjonn, "label") <- "Kjønn"

    cb <- make_codebook(dt)

    testthat::expect_true(
      inherits(cb, "htmlwidget")
    )
  # testthat::expect_s3_class(cb, "datatables")
    testthat::expect_true(inherits(cb, "htmlwidget"))
  }
)


testthat::test_that(
  "codebook alias returns same result class as make_codebook",
  {

    dt <- data.table::data.table(
      kjonn = haven::labelled(
        c(1, 2),
        labels = c(Mann = 1, Kvinne = 2)
      )
    )

    attr(dt$kjonn, "label") <- "Kjønn"

    cb1 <- make_codebook(dt)
    cb2 <- codebook(dt)

    testthat::expect_identical(
      class(cb1),
      class(cb2)
    )
  }
)


testthat::test_that(
  "save TRUE writes html file",
  {

    old <- setwd(tempdir())
    on.exit(setwd(old), add = TRUE)

    dt <- data.table::data.table(
      kjonn = haven::labelled(
        c(1, 2),
        labels = c(Mann = 1, Kvinne = 2)
      )
    )

    attr(dt$kjonn, "label") <- "Kjønn"

    make_codebook(
      d = dt,
      save = TRUE
    )

    testthat::expect_true(
      file.exists("dt.html")
    )
  }
)

testthat::test_that("page argument is passed to datatable", {

  dt <- data.table::data.table(
    kjonn = haven::labelled(
      c(1, 2),
      labels = c(Mann = 1, Kvinne = 2)
    )
  )

  attr(dt$kjonn, "label") <- "Kjønn"

  cb <- make_codebook(dt, page = 50)

  testthat::expect_equal(
    cb$x$options$pageLength,
    50
  )
})
