context("summarize functions")
skip_on_travis()
skip_on_cran()
skip_if_not_installed("datacampr")
library(dplyr)
library(datacampr)

simple_example <- tibble::tribble(
  ~"alternative.name", ~"timestamp.x", ~"timestamp.y",
  "control", "2018-07-10", NA,
  "control", "2018-07-11", NA,
  "control", "2018-07-12", NA,
  "treatment", "2018-07-01", "2018-07-05",
  "treatment", "2018-07-01", "2018-07-05",
  "control", "2018-07-01", NA,
  "control", "2018-07-02", NA,
  "control", "2018-07-03", NA,
  "treatment", "2018-07-01", NA,
  "treatment", "2018-07-01", "2018-07-05"
)

simple_summarized_conversion <- simple_example %>%
  summarize_conversions(timestamp.y)

test_that("summarize_conversions works with when group has no conversions", {
  expect_equal(nrow(simple_summarized_conversion), 2)
  expect_gt(tbl_views_snowplow_experiment_starts() %>%
              summarize_conversions(user_id) %>%
              filter(nb_conversions == 0) %>%
              collect() %>%
              nrow(), 10)
})


