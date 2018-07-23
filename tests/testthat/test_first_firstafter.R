context("first-firstafter joining")
library(dplyr)

landed <- tribble(
  ~user_id, ~timestamp,
  1, "2018-07-01",
  2, "2018-07-01",
  3, "2018-07-02",
  4, "2018-07-01",
  4, "2018-07-04",
  5, "2018-07-10",
  5, "2018-07-12",
  6, "2018-07-07",
  6, "2018-07-08"
)

registered <- tribble(
  ~user_id, ~timestamp,
  1, "2018-07-02",
  3, "2018-07-02",
  4, "2018-06-10",
  4, "2018-07-02",
  5, "2018-07-11",
  6, "2018-07-10",
  6, "2018-07-11",
  7, "2018-07-07"
)

test_that("after_join works with mode = inner and type = first-firstafter", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "inner", type = "first-firstafter")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x))
  expect_equal(length(res$user_id), n_distinct(res$user_id))
  expect_true(4 %in% res$user_id)
  expect_true(1 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = left and type = first-firstafter", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "left", type = "first-firstafter")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x |
                    is.na(res$timestamp.y)))
  expect_equal(length(res$user_id), n_distinct(res$user_id))
  expect_gt(nrow(landed), nrow(res))
  expect_true(4 %in% res$user_id)
  expect_true(1 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = right and type = first-firstafter", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "right", type = "first-firstafter")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x |
                    is.na(res$timestamp.x)))
  expect_equal(n_distinct(registered$user_id), n_distinct(res$user_id))
  expect_equal(nrow(res), nrow(registered))
  expect_true(4 %in% res$user_id)
  expect_true(1 %in% res$user_id)
  expect_true(!2 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(any(is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$user_id)))
})
