context("optional_funnel")
suppressPackageStartupMessages(library(dplyr))

multi_moments <- tibble::tribble(
  ~user_id, ~timestamp, ~ moment_type,
  1, "2018-07-01", "course_start",
  1, "2018-07-04", "project_start",
  2, "2018-07-01", "course_start",
  2, "2018-06-05", "project_start",
  3, "2018-07-02", "course_start",
  4, "2018-07-01", "course_start",
  4, "2018-07-04", "subscription",
  5, "2018-07-10", "course_start",
  5, "2018-07-12", "subscription",
  5, "2018-07-15", "project_start",
  6, "2018-07-07", "subscription",
  6, "2018-07-08", 'course_start',
  6, "2018-07-11", 'project_start'
) %>%
  mutate(timestamp = as.Date(timestamp))

test_that("Optional funnel works", {
  optional <- multi_moments %>%
    funnel_start("subscription", moment_type, timestamp, user_id) %>%
    funnel_step("course_start", type = "first-firstafter", optional = TRUE) %>%
    funnel_step("project_start", type = "first-firstafter")

  expect_true(all(optional$timestamp_course_start >= optional$timestamp_subscription, na.rm = TRUE))
  expect_true(all(optional$timestamp_project_start >= optional$timestamp_subscription, na.rm = TRUE))
  expect_true(all(optional$timestamp_project_start >= optional$timestamp_course_start, na.rm = TRUE))

  expect_equal(nrow(optional), 3)
  expect_equal(sum(is.na(optional$timestamp_course_start)), 2)
  expect_equal(sum(is.na(optional$timestamp_project_start)), 1)

  not_optional <- multi_moments %>%
    funnel_start("subscription", moment_type, timestamp, user_id) %>%
    funnel_step("course_start", type = "first-firstafter") %>%
    funnel_step("project_start", type = "first-firstafter")

  expect_equal(nrow(not_optional), 3)
  expect_equal(sum(is.na(not_optional$timestamp_course_start)), 2)
  expect_equal(sum(is.na(not_optional$timestamp_project_start)), 2)
})

test_that("Funnels don't allow multiple optional steps in a row", {
  setup <- multi_moments %>%
    funnel_start("subscription", moment_type, timestamp, user_id) %>%
    funnel_step("course_start", type = "first-firstafter", optional = TRUE)

  expect_error(funnel_step(setup, "project_start", type = "first-firstafter", optional = TRUE), "in a row")
})

test_that("Funnels with optional steps don't support duplicated user-timestamp pairs", {
  # TODO
})
