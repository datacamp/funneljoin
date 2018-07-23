library(datacampr)

soft_launches <- tbl_main_course_state_logs() %>%
  filter(new_state == "soft_launch") %>%
  select(course_id, soft_launch_at = created_at)

hard_launches <- tbl_main_course_state_logs() %>%
  filter(new_state == "live") %>%
  select(course_id, live_at = created_at)


test_that("after_join works for out-of-memory tables with mode = inner and type = first-first", {

  res <- after_join(soft_launches,
                    hard_launches,
                    by_user = "course_id",
                    by_time = c("soft_launch_at" = "live_at")) %>%
    collect()

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("course_id", "soft_launch_at", "live_at"))
  expect_true(all(res$soft_launch_at >= res$live_at))
  expect_equal(length(res$course_id), n_distinct(res$course_id))
  expect_true(nrow(res) >= 4)
  expect_true(all(!is.na(res$soft_launch_at)))
  expect_true(all(!is.na(res$live_at)))
  expect_true(all(!is.na(res$course_id)))
})




# # Is the left col always the first by course_id?
# first_soft_launches <- soft_launches %>%
#   group_by(course_id) %>%
#   filter(soft_launch_at == min(soft_launch_at, na.rm = T)) %>%
#   ungroup() %>%
#   collect() %>%
#   rename(first_soft_launch_at = soft_launch_at)
#
# test_results %>%
#   left_join(first_soft_launches, by = "course_id") %>%
#   count(first_soft_launch_at == soft_launch_at)
#
