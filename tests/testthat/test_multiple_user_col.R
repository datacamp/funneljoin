context("multi user column joining")
library(dplyr)

landed_multi <- landed %>%
  dplyr::mutate(package = c("dplyr", "tidyr", "dplyr", "tidyr", "dplyr", "tidyr", "tidyr", "dplyr", "dplyr"))

registered_multi <- registered %>%
  dplyr::mutate(package = c("tidyr", "dplyr", "dplyr", "tidyr", "dplyr", "dplyr", "purrr", "tidyr"),
         timestamp = timestamp + 1)

test_that("multiple join columns work with first-firstafter and left_join", {
  res <- landed_multi %>%
    after_left_join(registered_multi,
                    by_user = c("user_id", "package"),
                    by_time = "timestamp",
                    type = "first-firstafter")
  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id_package", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y >= res$timestamp.x |
                    is.na(res$timestamp.x) |
                    is.na(res$timestamp.y)))
  expect_true(any(is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$user_id_package)))
  expect_true("1_dplyr" %in% res$user_id_package)
  expect_true(!2 %in% res$user_id_package)
  expect_equal(sum(!is.na(res$timestamp.y)), 3)
})

test_that("multiple join columns work with first-firstafter and inner_join", {
  res <- landed_multi %>%
    after_inner_join(registered_multi,
                    by_user = c("user_id", "package"),
                    by_time = "timestamp",
                    type = "first-firstafter")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id_package", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y > res$timestamp.x |
                    is.na(res$timestamp.x) |
                    is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$user_id_package)))
  expect_true("4_tidyr" %in% res$user_id_package)
  expect_true(!2 %in% res$user_id_package)
  expect_true(!"6_purrr" %in% res$user_id_package)
  expect_true(!"1_dplyr" %in% res$user_id_package)

  expect_equal(nrow(res), 3)
})

test_that("multiple join columns work with any-any and inner_join", {
  res <- landed_multi %>%
    after_inner_join(registered_multi,
                     by_user = c("user_id", "package"),
                     by_time = "timestamp",
                     type = "any-any")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id_package", "timestamp.x", "timestamp.y"))
  expect_true(all(res$timestamp.y > res$timestamp.x |
                    is.na(res$timestamp.x) |
                    is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$timestamp.y)))
  expect_true(all(!is.na(res$timestamp.x)))
  expect_true(all(!is.na(res$user_id_package)))
  expect_true("4_tidyr" %in% res$user_id_package)
  expect_true(!2 %in% res$user_id_package)
  expect_true(!"1_dplyr" %in% res$user_id_package)
  expect_equal(nrow(res), 4)
  expect_equal(nrow(dplyr::filter(res, user_id_package == "6_dplyr")), 2)
})


test_that("multiple join columns work with any-any and anti_join", {
  res <- landed_multi %>%
    after_join(registered_multi,
                     by_user = c("user_id", "package"),
                     by_time = "timestamp",
                     type = "any-any",
               mode = "anti")

  expect_is(res, "tbl_df")
  expect_equal(names(res), c("user_id_package", "timestamp"))
  expect_true(all(!is.na(res$timestamp)))
  expect_true(all(!is.na(res$user_id_package)))
  expect_true(!"4_tidyr" %in% res$user_id_package)
  expect_true(!2 %in% res$user_id_package)
  expect_true("1_dplyr" %in% res$user_id_package)
  expect_equal(nrow(res), 5)
  expect_equal(nrow(dplyr::filter(res, user_id_package == "5_tidyr")), 2)
})


