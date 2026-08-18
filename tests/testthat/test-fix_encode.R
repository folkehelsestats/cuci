test_that("raw Latin-1/Windows-1252 bytes are fixed", {
  expect_equal(fix_encode("Sm\xe5 tr\xf8ndersk kr\xe6mmerhus"),
               "Sm\u00e5 tr\u00f8ndersk kr\u00e6mmerhus")

  expect_equal(fix_encode("\xc5rsrapport"), "\u00c5rsrapport")   # AA
  expect_equal(fix_encode("\xc6rsrapport"), "\u00c6rsrapport")   # AE
  expect_equal(fix_encode("\xd8konomi"),    "\u00d8konomi")      # OE
})

test_that("double-encoded UTF-8 sequences are fixed", {
  expect_equal(fix_encode("Sm\u00c3\u00a5 tr\u00c3\u00b8ndersk kr\u00c3\u00a6mmerhus"),
               "Sm\u00e5 tr\u00f8ndersk kr\u00e6mmerhus")

  expect_equal(fix_encode("\u00c3\u2020rtig"), "\u00c6rtig")  # AE
})

test_that("lowercase letters are handled: ae, aa, oe", {
  expect_equal(fix_encode("kr\xe6mmerhus"), "kr\u00e6mmerhus")
  expect_equal(fix_encode("sm\xe5"),        "sm\u00e5")
  expect_equal(fix_encode("tr\xf8ndersk"),  "tr\u00f8ndersk")
})

test_that("uppercase letters are handled: AE, AA, OE", {
  expect_equal(fix_encode("\xc6rlig"),  "\u00c6rlig")
  expect_equal(fix_encode("\xc5rlig"),  "\u00c5rlig")
  expect_equal(fix_encode("\xd8yene"),  "\u00d8yene")
})

test_that("stray \\xed byte is converted to plain i", {
  expect_equal(fix_encode("m\xedn"), "min")
})

test_that("strings without mangled bytes are left unchanged", {
  expect_equal(fix_encode("Oslo"), "Oslo")
  expect_equal(fix_encode("Bergen kommune"), "Bergen kommune")
  expect_equal(fix_encode(NA_character_), NA_character_)
  expect_equal(fix_encode(""), "")
})

test_that("vectorised input is handled element-wise", {
  input    <- c("Sm\xe5", "tr\xf8ndersk", "kr\xe6mmerhus", "Oslo")
  expected <- c("Sm\u00e5", "tr\u00f8ndersk", "kr\u00e6mmerhus", "Oslo")
  expect_equal(fix_encode(input), expected)
})

test_that("output length matches input length", {
  input <- c("Sm\xe5", "\xc6rlig", "Oslo", NA_character_)
  expect_length(fix_encode(input), length(input))
})

test_that("multiple mangled characters in one string are all fixed", {
  expect_equal(fix_encode("\xc6r\xe5 og \xf8y"), "\u00c6r\u00e5 og \u00f8y")
})
