context("first-first joining")
library(dplyr)

test_that("after_join works with mode = inner and type = first-first and ties = TRUE", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"),
                    type = "first-first", mode = "inner", ties = TRUE)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x))
  expect_equal(length(res$user_id), dplyr::n_distinct(res$user_id))
  expect_true(nrow(res) >= 4)
  expect_true(1 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
  expect_true(!4 %in% res$user_id)
})

test_that("after_join works with mode = left and type = first-first", {

  res <- after_join(landed, registered, mode = "left", by_user = "user_id", by_time = c("timestamp" = "timestamp"), type = "first-first")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x | is.na(res$timestamp.y)))
  expect_equal(length(res$user_id), n_distinct(res$user_id))
  expect_equal(nrow(res), n_distinct(landed$user_id))
  expect_true(1 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = right and type = first-first", {

  res <- after_join(landed, registered, mode = "right", by_user = "user_id", by_time = c("timestamp" = "timestamp"), type = "first-first")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x | is.na(res$timestamp.x)))
  expect_equal(length(res$user_id), n_distinct(res$user_id))
  expect_equal(nrow(res), dplyr::n_distinct(registered$user_id))
  expect_true(1 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(any(is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = anti and type = first-first", {

  res <- after_join(landed, registered, mode = "anti", by_user = "user_id", by_time = c("timestamp" = "timestamp"), type = "first-first")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp"))
  expect_true(4 %in% res$user_id)
  expect_true(2 %in% res$user_id)
  expect_true(3 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = semi and type = first-first", {

  res <- after_join(landed, registered, mode = "semi", by_user = "user_id", by_time = c("timestamp" = "timestamp"), type = "first-first")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp"))
  expect_true(1 %in% res$user_id)
  expect_true(!2 %in% res$user_id)
  expect_true(!3 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = full and type = first-first", {

  res <- after_join(landed, registered, mode = "full", by_user = "user_id", by_time = c("timestamp" = "timestamp"), type = "first-first")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x |
                    is.na(res$timestamp.x) |
                    is.na(res$timestamp.y)))
  expect_gt(nrow(res), dplyr::n_distinct(landed$user_id))
  expect_gt(nrow(res), dplyr::n_distinct(registered$user_id))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(any(is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$user_id)))
})

