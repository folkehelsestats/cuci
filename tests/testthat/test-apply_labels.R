testthat::test_that(
  "apply_labels adds variable and value labels",
  {

    cfg <- structure(list(
      label_lookup = list(
        kjonn = "IOs kjønn"
      ),
      value_map = list(
        kjonn = data.frame(
          num_value = c(1L, 2L),
          chr_value = c(
            "Mann",
            "Kvinne"
          )
        )
      )
    ), class = "cc_config")

    df <- data.frame(
      kjonn = c(1L, 2L, 1L)
    )

    res <- apply_labels(df, cfg)

    testthat::expect_s3_class(
      res$kjonn,
      "haven_labelled"
    )

    testthat::expect_equal(
      attr(res$kjonn, "label"),
      "IOs kjønn"
    )

    testthat::expect_equal(
      attr(res$kjonn, "labels"),
      c(
        Mann = 1L,
        Kvinne = 2L
      )
    )
  }
)

testthat::test_that(
  "apply_labels adds variable label when value_map is NULL",
  {

    cfg <- structure(list(
      label_lookup = list(
        alder = "IOs alder"
      ),
      value_map = list(
        alder = NULL
      )
    ), class = "cc_config")

    df <- data.frame(
      alder = c(20, 30, 40)
    )

    res <- apply_labels(df, cfg)

    testthat::expect_equal(
      attr(res$alder, "label"),
      "IOs alder"
    )

    testthat::expect_false(
      inherits(
        res$alder,
        "haven_labelled"
      )
    )
  }
)

testthat::test_that(
  "strict TRUE errors when undefined values exist",
  {

    cfg <- structure(list(
      label_lookup = list(
        yrkesstatus = "Er du yrkesaktiv?"
      ),
      value_map = list(
        yrkesstatus = data.frame(
          num_value = c(
            1L,
            2L,
            8L,
            9L
          ),
          chr_value = c(
            "Ja",
            "Nei",
            "Ikke svar",
            "Vet ikke"
          )
        )
      )
    ), class = "cc_config")

    df <- data.frame(
      yrkesstatus = c(
        "1",
        "2",
        "D",
        "R"
      ),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      apply_labels(
        df,
        cfg,
        strict = TRUE
      ),
      "contains values not defined"
    )
  }
)

testthat::test_that(
  "strict FALSE converts undefined values to NA",
  {

    cfg <- structure(list(
      label_lookup = list(
        yrkesstatus = "Er du yrkesaktiv?"
      ),
      value_map = list(
        yrkesstatus = data.frame(
          num_value = c(
            1L,
            2L,
            8L,
            9L
          ),
          chr_value = c(
            "Ja",
            "Nei",
            "Ikke svar",
            "Vet ikke"
          )
        )
      )
    ), class = "cc_config")

    df <- data.frame(
      yrkesstatus = c(
        "1",
        "2",
        "D",
        "R"
      ),
      stringsAsFactors = FALSE
    )

    testthat::expect_warning(

      res <- apply_labels(
        df,
        cfg,
        strict = FALSE
      ),

      "converted to NA"
    )

    
    testthat::expect_equal(
      unname(as.vector(res$yrkesstatus)),
      c(1L, 2L, NA_integer_, NA_integer_)
    )

    testthat::expect_equal(
      attr(res$yrkesstatus, "label"),
      "Er du yrkesaktiv?"
    )

    testthat::expect_equal(
      attr(res$yrkesstatus, "labels"),
      c(
        Ja = 1L,
        Nei = 2L,
        "Ikke svar" = 8L,
        "Vet ikke" = 9L
      )
    )
  }
)

testthat::test_that(
  "apply_labels supports character value labels",
  {

    cfg <- structure(list(
      label_lookup = list(
        tett_spredt = "Type bosted"
      ),
      value_map = list(
        tett_spredt = data.frame(
          num_value = c(
            "s",
            "t"
          ),
          chr_value = c(
            "Spredtbygd",
            "Tettbygd"
          ),
          stringsAsFactors = FALSE
        )
      )
    ), class = "cc_config")

    df <- data.frame(
      tett_spredt = c(
        "s",
        "t",
        "s"
      ),
      stringsAsFactors = FALSE
    )

    res <- apply_labels(df, cfg)

    testthat::expect_s3_class(
      res$tett_spredt,
      "haven_labelled"
    )

    testthat::expect_equal(
      attr(res$tett_spredt, "label"),
      "Type bosted"
    )

    testthat::expect_equal(
      attr(res$tett_spredt, "labels"),
      c(
        Spredtbygd = "s",
        Tettbygd = "t"
      )
    )
  }
)

testthat::test_that(
  "warning is issued when variable from config is missing",
  {

    cfg <- structure(list(
      label_lookup = list(
        alder = "IOs alder"
      ),
      value_map = list(
        alder = NULL
      )
    ), class = "cc_config")

    df <- data.frame(
      kjonn = c(1L, 2L)
    )

    testthat::expect_warning(
      apply_labels(df, cfg),
      "not found in data frame"
    )
  }
)
