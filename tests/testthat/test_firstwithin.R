context("firstwithin joining")
library(dplyr)

clicks <- tibble::tribble(
  ~ 'user', ~ 'timestamp',
  1, as.Date('2019-07-01'),
  1, as.Date('2019-07-30'),
  1, as.Date('2019-08-10'),
  2, as.Date('2019-08-10'),
  2, as.Date('2019-06-10'),
  3, as.Date('2019-08-24'),
  3, as.Date('2019-08-20'),
  3, as.Date('2019-06-01')
)

conversions <- tibble::tribble(
  ~ 'user', ~ 'timestamp',
  1, as.Date('2019-08-15'),
  2, as.Date('2019-08-05'),
  3, as.Date('2019-08-25'),
  3, as.Date('2019-08-30')
)

test_that("after_join works with mode = inner and type = firstwithin-any", {
  res <- clicks %>%
    after_inner_join(conversions,
                     by_time = "timestamp",
                     by_user = "user",
                     type = "firstwithin-any",
                     max_gap = as.difftime(30, units = "days"))

  expect_equal(nrow(res), 3)
  expect_equal(nrow(filter(res, user == 3)), 2)
})

test_that("after_join works with mode = inner and type = firstwithin-first", {
  res <- clicks %>%
    after_inner_join(conversions,
                     by_time = "timestamp",
                     by_user = "user",
                     type = "firstwithin-first",
                     max_gap = as.difftime(30, units = "days"))

  expect_equal(nrow(res), 2)
  expect_equal(filter(res, user == 3)$timestamp.y, as.Date("2019-08-25"))
  expect_true(!2 %in% res$user)
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$timestamp.y)))
})

test_that("after_join works with mode = right and type = firstwithin-any", {
  res <- clicks %>%
    after_right_join(conversions,
                     by_time = "timestamp",
                     by_user = "user",
                     type = "firstwithin-any",
                     max_gap = as.difftime(30, units = "days"),
                     gap_col = TRUE)

  expect_equal(nrow(res), 4)
  expect_true(is.na(filter(res, user == 2)$timestamp.x))
  expect_equal(nrow(filter(res, .gap > 60 * 60 * 24 * 30)), 0)

  res <- clicks %>%
    after_right_join(conversions,
                     by_time = "timestamp",
                     by_user = "user",
                     type = "firstwithin-any",
                     max_gap = as.difftime(90, units = "days"),
                     gap_col = TRUE)
  expect_equal(nrow(res), 4)
  expect_false(is.na(filter(res, user == 2)$timestamp.x))
  expect_equal(nrow(filter(res, .gap > 60 * 60 * 24 * 90)), 0)
})



