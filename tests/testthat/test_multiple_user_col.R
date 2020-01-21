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

logs <- tribble(
  ~date, ~event,
  "2020-01-06", "upload",
  "2020-01-08", "print",
  "2020-01-13", "upload",
  "2020-01-20", "print",
  "2020-01-21", "upload"
) %>%
  mutate(date = as.Date(date)) %>%
  mutate(user = 1)

logs <- tribble(
  ~date, ~event, ~deadline,
  "2020-01-06", "upload", "2020-01-05",
  "2020-01-08", "print", "2020-01-05",
  "2020-01-13", "upload", "2020-01-12",
  "2020-01-20", "print", "2020-01-19",
  "2020-01-21", "upload", "2020-01-19"
) %>%
  mutate(date = as.Date(date),
         user = 1,
         deadline = as.Date(deadline))

test_that("multiple join columns work with funnel_start and funnel_step", {
  res <- logs %>%
    funnel_start(moment_type = "upload", moment = "event",
                 tstamp = "date", user = c("deadline", "user")) %>%
    funnel_step(moment_type = "print", type = "any-firstafter")

  expect_is(res, "tbl_df")
  expect_is(res, "tbl_funnel")
  expect_equal(names(res), c("date_upload", "deadline_user", "date_print"))
  expect_true(all(!is.na(res$date_upload)))
  expect_true(all(!is.na(res$deadline_user)))
  expect_true(as.Date("2020-01-08") %in% res$date_print)
  expect_true(any(!is.na(res$date_print)))
  expect_equal(sum(is.na(res$date_print)), 2)

})

combined_landing_registration <- registered_multi %>%
  mutate(event = "registered") %>%
  bind_rows(landed_multi %>%
              mutate(event = "landed"))

test_that("multiple join columns work with funnel_start and funnel_step first-firstafter", {
  res_sequence <- combined_landing_registration %>%
    funnel_start(moment_type = "landed", moment = "event",
                 tstamp = "timestamp", user = c("user_id", "package")) %>%
    funnel_step(moment_type = "registered", type = "first-firstafter")

  res_normal <- landed_multi %>%
    after_left_join(registered_multi,
                    by_user = c("user_id", "package"),
                    by_time = "timestamp",
                    type = "first-firstafter",
                    suffix = c("_landed", "_registered"))

  expect_equal(res_normal, res_sequence)
  expect_is(res_sequence, "tbl_df")
  expect_is(res_sequence, "tbl_funnel")
  expect_equal(attributes(res_sequence)$funnel_metadata$user, "user_id_package")
  expect_equal(names(res_sequence), c("user_id_package", "timestamp_landed", "timestamp_registered"))
  expect_true(all(!is.na(res_sequence$timestamp_landed)))
  expect_true(any(is.na(res_sequence$timestamp_registered)))
  expect_true(as.Date("2018-07-11") %in% res_sequence$timestamp_registered)
  expect_true("1_dplyr" %in% res_sequence$user_id_package)

})

