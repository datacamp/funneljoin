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
  3, "2019-09-23", "D"
)

# join-col, inner, and first-firstafter
# 1-1, 3-4
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


