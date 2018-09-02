library(testthat)
library(fars)
library(readr)

test_that("Data frame imported", {
  dat <- fars_read(readr_example("mtcars.csv"))
  expect_that(dat, is_a('data.frame'))
})
