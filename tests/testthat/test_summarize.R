context("summarize functions")
library(dplyr)

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
  group_by(alternative.name) %>%
  summarize_conversions(timestamp.y)

for_conversion <- tibble::tribble(
  ~"experiment_group", ~"first_event", ~"last_event", ~"type",
  "control", "2018-07-01", NA, "click",
  "control", "2018-07-02", NA, "click",
  "control", "2018-07-03", "2018-07-05", "click",
  "treatment", "2018-07-01", "2018-07-05", "click",
  "treatment", "2018-07-01", "2018-07-05", "click",
  "control", "2018-07-01", NA, "purchase",
  "control", "2018-07-02", NA, "purchase",
  "control", "2018-07-03", NA, "purchase",
  "treatment", "2018-07-01", NA, "purchase",
  "treatment", "2018-07-01", "2018-07-05", "purchase"
)

converted_data <- for_conversion %>%
  group_by(type, experiment_group) %>%
  summarize_conversions(last_event)

test_that("summarize_conversions works with when group has no conversions", {
  expect_equal(nrow(simple_summarized_conversion), 2)
})

test_that("summarize_conversions won't return NA", {
  expect_equal(simple_summarized_conversion %>%
                 filter(alternative.name == "treatment") %>%
                 pull(nb_conversions), 3)
  expect_false(any(is.na(converted_data$nb_conversions)))
})

conversions_logical <- tibble::tribble(
  ~"experiment_group", ~"first_event", ~"last_event", ~"type",
  "control", "2018-07-01", FALSE, "click",
  "control", "2018-07-02", FALSE, "click",
  "control", "2018-07-03", TRUE, "click",
  "treatment", "2018-07-01", TRUE, "click",
  "treatment", "2018-07-01", TRUE, "click",
  "control", "2018-07-01", FALSE, "purchase",
  "control", "2018-07-02", FALSE, "purchase",
  "control", "2018-07-03", FALSE, "purchase",
  "treatment", "2018-07-01", FALSE, "purchase",
  "treatment", "2018-07-01", TRUE, "purchase"
)

summarized_logical_conversions <- conversions_logical %>%
  group_by(experiment_group) %>%
  summarize_conversions(last_event)

test_that("summarize_conversions works with TRUE/FALSE", {
  expect_equal(sum(summarized_logical_conversions$nb_conversions), 4)
})

true_and_na <- tibble::tribble(
  ~"alternative.name", ~"timestamp.x", ~"timestamp.y",
  "control", "2018-07-10", NA,
  "control", "2018-07-11", NA,
  "control", "2018-07-12", NA,
  "treatment", "2018-07-01", TRUE,
  "treatment", "2018-07-01", TRUE,
  "control", "2018-07-01", NA,
  "control", "2018-07-02", NA,
  "control", "2018-07-03", TRUE,
  "treatment", "2018-07-01", NA,
  "treatment", "2018-07-01", TRUE
)

true_and_na_summarized <- true_and_na %>%
  group_by(alternative.name) %>%
  summarize_conversions(timestamp.y)

test_that("summarize_conversions works with TRUE/NA", {
  expect_equal(sum(true_and_na_summarized$nb_conversions), 4)
})