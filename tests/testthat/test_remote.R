context("Joining using remote tables")
skip_on_travis()
skip_on_cran()
skip_if_not_installed("datacampr")
skip_if_not_installed("dcmetrics")
library(dcmetrics)
library(datacampr)

three_days = as.difftime(3, unit = "days")
three_days_numeric = 60 * 60 * 24 * 3

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
    dplyr::filter(course_id %in% !!res$course_id) %>%
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
    dplyr::filter(course_id %in% !!res$course_id) %>%
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

test_that("after_join works with mode = inner, type = first-firstafter, max_gap = numeric, gap_col is TRUE, and table is remote", {

  res <- after_join(soft_launches, hard_launches,
                    by_user = "course_id",
                    by_time = c("soft_launch_at" = "live_at"),
                    mode = "inner",
                    type = "first-firstafter",
                    max_gap = three_days_numeric, gap_col = TRUE) %>%
    dplyr::collect()

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("course_id", "soft_launch_at", ".gap", "live_at"))
  expect_true(all(res$live_at >= res$soft_launch_at))
  expect_gte(sum(res$.gap, na.rm = TRUE), 292755)
})

test_that("after_join works with mode = inner, type = any-any, table is remote", {
  # this was an analysis that broke in earlier version code
  # because of non-deterministic behavior of remote row_number

  x <- tbl_snowplow_events() %>%
    filter(collector_tstamp >= "2018-12-13",
           collector_tstamp <= "2018-12-17",
           event == "struct",
           se_label == "premium_intro_courses") %>%
    select(user_id = se_value, alternative.name = se_property, collector_tstamp) %>%
    mutate(experiment_start_date = DATE(collector_tstamp)) %>%
    mutate(user_id = as.integer(user_id))

  y <- tbl_views_b2x_user_subscriptions() %>%
    mutate(sub_start_date = DATE(started_at)) %>%
    filter(sub_start_date >= "2018-12-13",
           sub_start_date <= "2018-12-17")

  res <- x %>%
    after_inner_join(y,
                     by_user = "user_id",
                     by_time = c("experiment_start_date" = "sub_start_date"),
                     type = "any-any") %>%
    count() %>%
    pull(n)

  expect_gte(res, 50)
})


courses_started_datacamp <- tbl_views_user_content_history() %>%
  enrich_users(domain) %>%
  filter(domain == "datacamp.com",
        content_type == "course",
        between(started_at, "2019-08-01", "2019-08-20"))  %>%
  select(user_id, started_at)

projects_started_datacamp <- tbl_views_user_content_history() %>%
  enrich_users(domain) %>%
  filter(domain == "datacamp.com",
         content_type == "project",
         between(started_at, "2019-08-01", "2019-08-20")) %>%
  select(user_id, started_at)

courses_started_datacamp_local <- courses_started_datacamp %>%
  collect()

projects_started_datacamp_local <- projects_started_datacamp %>%
  collect()

courses_started_datacamp %>%
  after_left_join(projects_started_datacamp,
                  by_user = "user_id",
                  by_time = "started_at",
                  type = "first-any",
                  max_gap = as.difftime(10, units = "days"))

test_that("after_join results with mode = inner, type = firstwithin-any, table is remote is same when table is local", {

  res_remote <- courses_started_datacamp %>%
    after_inner_join(projects_started_datacamp,
                    by_user = "user_id",
                    by_time = "started_at",
                    type = "firstwithin-any",
                    max_gap = as.difftime(10, units = "days"),
                    gap_col = TRUE) %>%
    collect()

  res_local <- courses_started_datacamp_local %>%
    after_inner_join(projects_started_datacamp_local,
                    by_user = "user_id",
                    by_time = "started_at",
                    type = "firstwithin-any",
                    max_gap = as.difftime(10, units = "days"),
                    gap_col = TRUE) %>%
    collect()

  expect_equal(res_local, res_remote)
  expect_equal(nrow(res_remote), 17)
  expect_true(all(res_remote$.gap < 10 * 60 * 60 * 24))
  expect_equal(n_distinct(res_remote$started_at.y), nrow(res_remote))
  expect_equal(n_distinct(res_remote$started_at.x), 8)

})
