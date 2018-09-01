library(testthat)
library(fars)

test_that("Data frame imported", {
  library(readr)
  dat <- fars_read(readr_example("mtcars.csv"))
  expect_that(dat, is_a('data.frame'))
})
