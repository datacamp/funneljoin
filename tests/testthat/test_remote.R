context("Joining using remote tables")
skip_on_travis()
skip_on_cran()
skip_if_not_installed("datacampr")
library(datacampr)

soft_launches <- tbl_main_course_state_logs() %>%
  dplyr::filter(new_state == "soft_launch") %>%
  dplyr::select(course_id, soft_launch_at = created_at)

hard_launches <- tbl_main_course_state_logs() %>%
  dplyr::filter(new_state == "live") %>%
  dplyr::select(course_id, live_at = created_at)


test_that("after_join works for out-of-memory tables with mode = inner and type = first-first", {

  res <- after_join(soft_launches,
                    hard_launches,
                    by_user = "course_id",
                    by_time = c("soft_launch_at" = "live_at"),
                    type = "first-first") %>%
    dplyr::collect()

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("course_id", "soft_launch_at", "live_at"))
  expect_true(all(res$soft_launch_at <= res$live_at))
  expect_equal(length(res$course_id), dplyr::n_distinct(res$course_id))
  expect_true(nrow(res) >= 40)
  expect_true(all(!is.na(res$soft_launch_at)))
  expect_true(all(!is.na(res$live_at)))
  expect_true(all(!is.na(res$course_id)))

  first_soft_launches <- soft_launches %>%
    dplyr::group_by(course_id) %>%
    dplyr::summarise(first_soft_launch_at = min(soft_launch_at, na.rm = T)) %>%
    dplyr::collect() %>%
    inner_join(res, by = "course_id")

  expect_equal(first_soft_launches$first_soft_launch_at, first_soft_launches$soft_launch_at)
})


test_that("after_join works for out-of-memory tables with mode = inner and type = last-firstafter", {

  res <- after_join(soft_launches,
                    hard_launches,
                    by_user = "course_id",
                    by_time = c("soft_launch_at" = "live_at"),
                    mode = "inner",
                    type = "last-firstafter") %>%
    dplyr::collect()

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("course_id", "soft_launch_at", "live_at"))
  expect_true(all(res$soft_launch_at <= res$live_at))
  expect_equal(length(res$course_id), n_distinct(res$course_id))
  expect_true(nrow(res) >= 40)
  expect_true(all(!is.na(res$soft_launch_at)))
  expect_true(all(!is.na(res$live_at)))
  expect_true(all(!is.na(res$course_id)))

  last_soft_launches <- soft_launches %>%
    group_by(course_id) %>%
    summarise(last_soft_launch_at = max(soft_launch_at, na.rm = T)) %>%
    collect() %>%
    inner_join(res, by = "course_id")

  expect_equal(last_soft_launches$last_soft_launch_at, last_soft_launches$soft_launch_at)
})


test_that("after_join works for out-of-memory tables with mode = anti and type = last-firstafter", {

  res <- after_join(soft_launches,
                    hard_launches,
                    by_user = "course_id",
                    by_time = c("soft_launch_at" = "live_at"),
                    mode = "anti",
                    type = "last-firstafter") %>%
    collect()

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("course_id", "soft_launch_at"))
  expect_equal(length(res$course_id), dplyr::n_distinct(res$course_id))
  expect_true(nrow(res) >= 1)
  expect_true(all(!is.na(res$soft_launch_at)))
  expect_true(all(!is.na(res$course_id)))

  hard_launches_in_res <- hard_launches %>%
    dplyr::filter(course_id %in% res$course_id) %>%
    dplyr::collect()

  expect_equal(nrow(hard_launches_in_res), 0)
})


test_that("after_join works for out-of-memory tables with mode = semi and type = first-any", {

  res <- after_join(soft_launches,
                    hard_launches,
                    by_user = "course_id",
                    by_time = c("soft_launch_at" = "live_at"),
                    mode = "semi",
                    type = "first-any") %>%
    collect()

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("course_id", "soft_launch_at"))
  expect_true(nrow(res) >= 40)
  expect_true(all(!is.na(res$soft_launch_at)))
  expect_true(all(!is.na(res$course_id)))

  hard_launches_in_res <- hard_launches %>%
    dplyr::filter(course_id %in% res$course_id) %>%
    dplyr::collect()

  expect_equal(n_distinct(hard_launches_in_res$course_id), nrow(res))
})


test_that("after_join works for out-of-memory tables with mode = inner and type = any-any with max_gap", {

  res <- after_join(soft_launches,
                    hard_launches,
                    by_user = "course_id",
                    by_time = c("soft_launch_at" = "live_at"),
                    type = "any-any",
                    max_gap = as.difftime(10, unit = "days")) %>%
    dplyr::collect()

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("course_id", "soft_launch_at", "live_at"))
  expect_true(all(res$soft_launch_at <= res$live_at))
  expect_equal(length(res$course_id), dplyr::n_distinct(res$course_id))
  expect_true(nrow(res) >= 15)
  expect_true(all(!is.na(res$soft_launch_at)))
  expect_true(all(!is.na(res$live_at)))
  expect_true(all(!is.na(res$course_id)))
  expect_true(all(as.difftime(res$live_at - res$soft_launch_at) <
                    as.difftime(10, unit = "days")))

  first_soft_launches <- soft_launches %>%
    dplyr::group_by(course_id) %>%
    dplyr::summarise(first_soft_launch_at = min(soft_launch_at, na.rm = T)) %>%
    dplyr::collect() %>%
    inner_join(res, by = "course_id")

  expect_equal(first_soft_launches$first_soft_launch_at, first_soft_launches$soft_launch_at)
})

test_that("after_join works for out-of-memory tables with mode = inner and type = withingap with numeric", {

  res <- after_join(soft_launches,
                    hard_launches,
                    by_user = "course_id",
                    by_time = c("soft_launch_at" = "live_at"),
                    type = "any-any",
                    max_gap = 1296000) %>%
    dplyr::collect()

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("course_id", "soft_launch_at", "live_at"))
  expect_true(all(res$soft_launch_at <= res$live_at))
  expect_equal(length(res$course_id), dplyr::n_distinct(res$course_id))
  expect_true(nrow(res) >= 25)
  expect_true(all(!is.na(res$soft_launch_at)))
  expect_true(all(!is.na(res$live_at)))
  expect_true(all(!is.na(res$course_id)))
  expect_true(all(as.difftime(res$live_at - res$soft_launch_at) < 1296000))

  first_soft_launches <- soft_launches %>%
    dplyr::group_by(course_id) %>%
    dplyr::summarise(first_soft_launch_at = min(soft_launch_at, na.rm = T)) %>%
    dplyr::collect() %>%
    inner_join(res, by = "course_id")

  expect_equal(first_soft_launches$first_soft_launch_at, first_soft_launches$soft_launch_at)
})

test_that("after_join throws an error when you try to do multiple remote after joins in a row", {
  expect_error(after_join(soft_launches,
                          hard_launches,
                          by_user = "course_id",
                          by_time = c("soft_launch_at" = "live_at"),
                          type = "first-first") %>%
                 after_join(hard_launches,
                            by_user = "course_id",
                            by_time = "live_at",
                            type = "first-any"))
})

