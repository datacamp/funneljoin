context("lastbefore-firstafter joining")
library(dplyr)

test_that("after_join works with mode = inner and type = lastbefore-firstafter", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "inner", type = "lastbefore-firstafter")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x))
  expect_equal(length(res$user_id), dplyr::n_distinct(res$user_id))
  expect_true(4 %in% res$user_id)
  expect_true(1 %in% res$user_id)
  expect_true(!(7 %in% res$user_id))
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
  expect_equal(filter(res, user_id == 6)$timestamp.x,
               as.Date("2018-07-08"))
  expect_equal(filter(res, user_id == 6)$timestamp.y,
               as.Date("2018-07-10"))
  expect_equal(filter(res, user_id == 5)$timestamp.x,
               as.Date("2018-07-10"))
})

test_that("after_join works with mode = left and type = lastbefore-firstafter", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "left", type = "lastbefore-firstafter")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x |
                    is.na(res$timestamp.y)))
  expect_equal(nrow(landed), nrow(res))
  expect_true(4 %in% res$user_id)
  expect_true(1 %in% res$user_id)
  expect_true(!(7 %in% res$user_id))
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
  expect_true(is.na(filter(res, user_id == 5 & timestamp.x ==
                        "2018-07-12")$timestamp.y))
  expect_true(is.na(filter(res, user_id == 2)$timestamp.y))
})

test_that("after_join works with mode = right and type = lastbefore-firstafter", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "right", type = "lastbefore-firstafter")

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
  expect_equal(filter(res, user_id == 6 &
                        timestamp.x == "2018-07-08")$timestamp.y,
               as.Date("2018-07-10"))
})

test_that("after_join works with mode = anti and type = lastbefore-firstafter", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "anti", type = "lastbefore-firstafter")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp"))
  expect_true(2 %in% res$user_id)
  expect_true(!3 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp)))
  expect_true(all(!is.na(res$user_id)))
  expect_true(as.Date("2018-07-04") %in% res$timestamp)
  expect_true(as.Date("2018-07-12") %in% res$timestamp)
})

test_that("after_join works with mode = semi and type = lastbefore-firstafter", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "semi", type = "lastbefore-firstafter")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp"))
  expect_true(!2 %in% res$user_id)
  expect_true(3 %in% res$user_id)
  expect_true(all(!is.na(res$timestamp)))
  expect_true(all(!is.na(res$user_id)))
  expect_true(as.Date("2018-07-10") %in% res$timestamp)
  expect_equal(filter(res, user_id == 6)$timestamp,
               as.Date("2018-07-08"))
})

test_that("after_join works with mode = full and type = lastbefore-firstafter", {

  res <- after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"), mode = "full", type = "lastbefore-firstafter")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id", "timestamp.x", "timestamp.y"))
  expect_true(nrow(res) >= 8)
  expect_true(all(res$timestamp.y >= res$timestamp.x |
                    is.na(res$timestamp.x) |
                    is.na(res$timestamp.y)))
  expect_gt(nrow(res), dplyr::n_distinct(landed$user_id))
  expect_gt(nrow(res), dplyr::n_distinct(registered$user_id))
  expect_true(any(is.na(res$timestamp.x)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$user_id)))
  expect_true(is.na(filter(res,
                            user_id == 4 &
                              timestamp.x == "2018-07-04")$timestamp.y))
  expect_equal(filter(res,
                      user_id == 6 &
                        timestamp.x == "2018-07-08")$timestamp.y,
               as.Date("2018-07-10"))
})


