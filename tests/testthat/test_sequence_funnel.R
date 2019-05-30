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

res_funnel_start <- funnel_start(moments, "course_start", moment_type, timestamp, user_id)

test_that("funnel_start works", {

  expect_is(res_funnel_start, "tbl_df")
  expect_equal(names(res_funnel_start), c("user_id", "timestamp_course_start", "moment_type_course_start"))
  expect_equal(nrow(res_funnel_start), 6)
  expect_equal(names(attributes(res_funnel_start)$funnel_metadata),
               c("original_data", "tstamp", "user", "event", "event_sequence"))
  expect_equal(attributes(res_funnel_start)$funnel_metadata$event_sequence, "course_start")
  expect_equal(moments, attributes(res_funnel_start)$funnel_metadata$original_data)
})

test_that("funnel_step works with one step first-firstafter", {
  res_one_step <- res_funnel_start %>%
    funnel_step("subscription", type = "first-firstafter")
  one_step_md <- attributes(res_one_step)$funnel_metadata

  expect_silent(res_funnel_start %>%
                  funnel_step("subscription", type = "first-firstafter"))
  expect_equal(one_step_md$event_sequence, c("course_start", "subscription"))
  expect_equal(unique(one_step_md$original_data$moment_type),
               c("course_start", "subscription"))
  expect_equal(names(res_one_step),
               c("user_id", "timestamp_course_start", "moment_type_course_start",
                 "timestamp_subscription", "moment_type_subscription"))
  expect_equal(res_one_step %>%
                 filter(timestamp_subscription < timestamp_course_start) %>%
                 nrow(), 0)

})
