context("max_gap")

three_days = as.difftime(3, unit = "days")
three_days_numeric = 60 * 60 * 24 * 3

test_that("after_join works with mode = left and type = any-any and max_gap = difftime", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "left", type = "any-any", max_gap = three_days)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x | is.na(res$timestamp.y)))
  expect_gt(length(res$user_id), dplyr::n_distinct(res$user_id))
  expect_gt(nrow(res), dplyr::n_distinct(landed$user_id))
  expect_true(1 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = left and type = any-any and max_gap = numeric", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "left", type = "any-any", max_gap = three_days_numeric)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x | is.na(res$timestamp.y)))
  expect_gt(length(res$user_id), dplyr::n_distinct(res$user_id))
  expect_gt(nrow(res), dplyr::n_distinct(landed$user_id))
  expect_true(1 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
})


test_that("after_join works with mode = inner and type = any-any and max_gap = difftime", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "inner", type = "any-any", max_gap = three_days)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(nrow(res) >= 4)
  expect_true(all(res$timestamp.y >= res$timestamp.x))
  expect_gt(n_distinct(landed$user_id), nrow(res))
  expect_true(1 %in% res$user_id)
  expect_true(!2 %in% res$user_id)
  expect_equal(nrow(dplyr::filter(res, user_id == 6)), 1)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = inner and type = any-any and max_gap = numeric", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "inner", type = "any-any", max_gap = three_days_numeric)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(nrow(res) >= 4)
  expect_true(all(res$timestamp.y >= res$timestamp.x))
  expect_gt(n_distinct(landed$user_id), nrow(res))
  expect_true(1 %in% res$user_id)
  expect_true(!2 %in% res$user_id)
  expect_equal(nrow(dplyr::filter(res, user_id == 6)), 1)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = right and type = any-any and max_gap = difftime", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "right", type = "any-any", max_gap = three_days)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x | is.na(res$timestamp.x)))
  expect_gt(length(res$user_id), n_distinct(res$user_id))
  expect_gt(nrow(res), n_distinct(landed$user_id))
  expect_true(1 %in% res$user_id)
  expect_true(!2 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(any(is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = right and type = any-any and max_gap = numeric", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "right", type = "any-any", max_gap = three_days_numeric)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x | is.na(res$timestamp.x)))
  expect_gt(length(res$user_id), n_distinct(res$user_id))
  expect_gt(nrow(res), n_distinct(landed$user_id))
  expect_true(1 %in% res$user_id)
  expect_true(!2 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(any(is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$user_id)))
})


test_that("after_join works with mode = anti and type = any-any and max_gap = difftime", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "anti", type = "any-any", max_gap = three_days,
                    ties = TRUE)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp"))
  expect_true(2 %in% res$user_id)
  expect_true(!3 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp)))
  expect_true(all(!is.na(res$user_id)))
  expect_true(as.Date("2018-07-04") %in% res$timestamp)
})

test_that("after_join works with mode = anti and type = any-any and max_gap = numeric and ties = TRUE", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "anti", type = "any-any", max_gap = three_days_numeric,
                    ties = TRUE)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp"))
  expect_true(2 %in% res$user_id)
  expect_true(!3 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp)))
  expect_true(all(!is.na(res$user_id)))
  expect_true(as.Date("2018-07-04") %in% res$timestamp)
})

test_that("after_join works with mode = semi and type = any-any and max_gap = difftime and ties = TRUE", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "semi", type = "any-any", max_gap = three_days,
                    ties = TRUE)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp"))
  expect_true(!2 %in% res$user_id)
  expect_true(3 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = semi and type = any-any and max_gap = numeric and ties = TRUE", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "semi", type = "any-any", max_gap = three_days_numeric, ties = TRUE)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp"))
  expect_true(!2 %in% res$user_id)
  expect_true(3 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp)))
  expect_true(all(!is.na(res$user_id)))
})


test_that("after_join works with mode = full and type = any-any and max_gap = difftime", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "full", type = "any-any", max_gap = three_days)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(nrow(res) >= 10)
  expect_true(all(res$timestamp.y >= res$timestamp.x |
                    is.na(res$timestamp.x) |
                    is.na(res$timestamp.y)))
  expect_gt(nrow(res), n_distinct(landed$user_id))
  expect_gt(nrow(res), n_distinct(registered$user_id))
  expect_equal(nrow(dplyr::filter(res, user_id == 6)), 3)
  expect_true(any(is.na(res$timestamp.x)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = full and type = any-any and max_gap = numeric", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "full", type = "any-any", max_gap = three_days_numeric)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(nrow(res) >= 10)
  expect_true(all(res$timestamp.y >= res$timestamp.x |
                    is.na(res$timestamp.x) |
                    is.na(res$timestamp.y)))
  expect_gt(nrow(res), n_distinct(landed$user_id))
  expect_gt(nrow(res), n_distinct(registered$user_id))
  expect_equal(nrow(dplyr::filter(res, user_id == 6)), 3)
  expect_true(any(is.na(res$timestamp.x)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = left, type = any-any, max_gap = difftime, and gap_col is TRUE", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "left", type = "any-any", max_gap = three_days, gap_col = TRUE)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", ".gap", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x | is.na(res$timestamp.y)))
  expect_gt(length(res$user_id), dplyr::n_distinct(res$user_id))
  expect_gt(nrow(res), dplyr::n_distinct(landed$user_id))
  expect_true(1 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
})

test_that("after_join works with mode = left, type = any-any, max_gap = numeric, and gap_col is TRUE", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "left", type = "any-any", max_gap = three_days_numeric, gap_col = TRUE)

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", ".gap", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x | is.na(res$timestamp.y)))
  expect_gt(length(res$user_id), dplyr::n_distinct(res$user_id))
  expect_gt(nrow(res), dplyr::n_distinct(landed$user_id))
  expect_true(1 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
  expect_equal(sum(res$.gap, na.rm = TRUE), 432000)
})

