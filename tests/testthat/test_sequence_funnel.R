context("sequence_funnel")
library(dplyr)

moments <- tibble::tribble(
  ~user_id, ~timestamp, ~ moment_type,
  1, "2018-07-01", "course_start",
  2, "2018-07-01", "course_start",
  3, "2018-07-02", "course_start",
  4, "2018-07-01", "course_start",
  4, "2018-07-04", "subscription",
  5, "2018-07-10", "course_start",
  5, "2018-07-12", "subscription",
  6, "2018-07-07", "subscription",
  6, "2018-07-08", 'course_start'
) %>%
  mutate(timestamp = as.Date(timestamp))

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

res_funnel_start <- funnel_start(moments, moment_type = "course_start",
                                 moment = "moment_type", tstamp = "timestamp", user = "user_id")

test_that("funnel_start works", {

  expect_is(res_funnel_start, "tbl_df")
  expect_equal(names(res_funnel_start), c("user_id", "timestamp_course_start"))
  expect_equal(nrow(res_funnel_start), 6)
  expect_equal(names(attributes(res_funnel_start)$funnel_metadata),
               c("original_data", "tstamp", "user", "moment", "moment_sequence", "type_sequence"))
  expect_equal(attributes(res_funnel_start)$funnel_metadata$moment_sequence, "course_start")
  expect_equal(moments, attributes(res_funnel_start)$funnel_metadata$original_data)
  expect_equal(length(res_funnel_start$user_id), dplyr::n_distinct(res_funnel_start$user_id))

})

test_that("funnel_step works with one step first-firstafter", {
  res_one_step <- res_funnel_start %>%
    funnel_step("subscription", type = "first-firstafter")
  one_step_md <- attributes(res_one_step)$funnel_metadata

  expect_silent(res_funnel_start %>%
                  funnel_step("subscription", type = "first-firstafter"))
  expect_equal(one_step_md$moment_sequence, c("course_start", "subscription"))
  expect_equal(unique(one_step_md$original_data$moment_type),
               c("course_start", "subscription"))
  expect_equal(names(res_one_step),
               c("user_id", "timestamp_course_start",
                 "timestamp_subscription"))
  expect_equal(res_one_step %>%
                 filter(timestamp_subscription < timestamp_course_start) %>%
                 nrow(), 0)
  expect_equal(moments, attributes(res_one_step)$funnel_metadata$original_data)

})

test_that("funnel_step works with two steps first-firstafter", {
  res_two_step <- multi_moments %>%
    funnel_start("course_start", "moment_type", "timestamp", "user_id") %>%
    funnel_step("subscription", type = "first-firstafter") %>%
    funnel_step("project_start", type = "first-firstafter")
  two_step_md <- attributes(res_two_step)$funnel_metadata

  expect_equal(two_step_md$moment_sequence, c("course_start", "subscription", "project_start"))
  expect_equal(names(res_two_step),
               c("user_id", "timestamp_course_start",
                 "timestamp_subscription",
                 "timestamp_project_start"))
  expect_true(any(!is.na(res_two_step$timestamp_project_start)))
  expect_equal(res_two_step %>%
                 filter(timestamp_project_start < timestamp_subscription) %>%
                 nrow(), 0)
  expect_equal(multi_moments, attributes(res_two_step)$funnel_metadata$original_data)
  expect_equal(two_step_md$type_sequence, c("first-firstafter", "first-firstafter"))

})

test_that("funnel_step works when there use moment multiple times", {
  res_start_step <- multi_moments %>%
    funnel_start("course_start", "moment_type", "timestamp", "user_id") %>%
    funnel_step("subscription", type = "first-firstafter")

  expect_error(res_repeat_step %>%
                 funnel_step("subscription", type = "first-firstafter"))

  res_repeat_step <- multi_moments %>%
    funnel_start("course_start", "moment_type", "timestamp", "user_id") %>%
    funnel_step("subscription", type = "first-firstafter") %>%
    funnel_step("subscription", type = "first-firstafter", name = "sub_two")

  expect_equal(colnames(res_repeat_step), c("user_id", "timestamp_course_start",
                                            "timestamp_subscription", "timestamp_sub_two"))

  expect_equal(sum(!is.na(res_repeat_step$timestamp_sub_two)), 2)
})

test_that("group_by() preserves funnel attributes", {
  res_one_step_groups <- res_funnel_start %>%
    mutate(type = sample(c("a", "b"), nrow(res_funnel_start), replace = TRUE)) %>%
    funnel_step("subscription", type = "first-firstafter")

  md_grouped <- res_one_step_groups %>%
    group_by(type) %>%
    funneljoin:::funnel_metadata()

  expect_equal(md_grouped$tstamp, "timestamp")
  expect_equal(md_grouped$type_sequence, "first-firstafter")
  expect_equal(names(md_grouped), c("original_data", "tstamp", "user",
                                    "moment", "moment_sequence", "type_sequence", "optional_sequence"))
})
