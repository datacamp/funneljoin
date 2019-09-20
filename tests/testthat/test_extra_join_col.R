context("extra join column")
library(dplyr)

ad_views <- tibble::tribble(
  ~ "user", ~ "tstamp", ~ "ad",
  1, "2019-09-06", "A",
  1, "2019-09-08", "A",
  2, "2019-09-10", "A",
  2, "2019-09-20", "B",
  3, "2019-09-20", "C",
  4, "2019-09-25", "D"
)

ad_clicks <- tibble::tribble(
  ~ "user", ~ "tstamp", ~ "ad",
  1, "2019-09-07", "A",
  1, "2019-09-10", "A",
  2, "2019-09-25", "B",
  2, "2019-09-27", "A",
  3, "2019-09-23", "D",
  5, "2019-09-24", "C"
)

test_that("after_join works with mode = inner, a by_col, and type = first-firstafter", {
  res <- ad_views %>%
    after_inner_join(ad_clicks,
               type = "first-firstafter",
               by_time = "tstamp",
               by_user = "user",
               by_col = 'ad')

  expect_equal(nrow(res), 2)
  expect_equal(res$user, c(1, 2))
  expect_equal(res$ad.x, res$ad.y)
  expect_true(all(res$tstamp.y >= res$tstamp.x))
})


test_that("after_join works with mode = left, a by_col, and type = first-firstafter", {
  res <- ad_views %>%
    after_left_join(ad_clicks,
                     type = "first-firstafter",
                     by_time = "tstamp",
                     by_user = "user",
                     by_col = 'ad')

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user", "tstamp.x", "ad.x", "tstamp.y", "ad.y"))
  expect_true(all(!is.na(res$tstamp.x)))
  expect_true(any(is.na(res$tstamp.y)))
  expect_true(all(!is.na(res$ad.x)))
  expect_true(any(is.na(res$ad.y)))
  expect_equal(nrow(res), 4)
  expect_true(all(res$ad.x == res$ad.y | is.na(res$ad.y)))
  expect_true(all(res$tstamp.y >= res$tstamp.x | is.na(res$tstamp.y)))
  expect_gt(nrow(ad_views), nrow(res))
  expect_true(!5 %in% res$user)
})



