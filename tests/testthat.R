library(testthat)
library(fars)

test_that("Dataset name test", {
  filecl <- class(make_filename(2018))
  expect_that(filecl, equals('character'))
})
