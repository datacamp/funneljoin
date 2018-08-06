library(dplyr)
library(tibble)
library(lubridate)

x <- tribble(
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
) %>%
  mutate(timestamp = as.Date(timestamp))

y <- tribble(
  ~user_id, ~timestamp,
  1, "2018-07-02",
  3, "2018-07-02",
  4, "2018-06-10",
  4, "2018-07-02",
  5, "2018-07-11",
  6, "2018-07-10",
  6, "2018-07-11",
  7, "2018-07-07"
) %>%
  mutate(timestamp = as.Date(timestamp))

by_user = "user_id"
by_time = "timestamp"
user_xy <- dplyr:::common_by(by_user, x, y)
time_xy <- dplyr:::common_by(by_time, x, y)

x_i <- x %>%
  mutate(..idx = row_number())

y_i <- y %>%
  mutate(..idy = row_number())

if (time_xy$x == time_xy$y) {
  time_xy <- list(x = paste0(time_xy$x, ".x"),
                  y = paste0(time_xy$y, ".y"))
}

pairs <- x_i %>%
  inner_join(y_i, by = user_xy) %>%
  filter(!!sym(time_xy$x) <= !!sym(time_xy$y))

pairs <- pairs %>%
  select(..idx, ..idy)

dt <- as.difftime(60, units = "hours")

## if local
pairs <- x_i %>%
  inner_join(y_i, by = user_xy) %>%
  filter(timestamp.x <= timestamp.y) %>%
  mutate(time_diff = difftime(!!sym(time_xy$y), !!sym(time_xy$x), "secs"))

## if not local
pairs <- x_i %>%
  inner_join(y_i, by = user_xy) %>%
  filter(timestamp.x <= timestamp.y) %>%
  mutate(time_diff = !!sym(time_xy$y) - !!sym(time_xy$x)) %>%

#### within gap

## if local
pairs %>%
  filter(time_diff < !!as_seconds(dt)) %>%
  select(..idx, ..idy)

# if not local
pairs %>%
  filter(time_diff < !!as_seconds(dt, sql = TRUE)) %>%
  select(..idx, ..idy)

### min gap

pairs <- pairs %>%
  group_by(user_id) %>%
  filter(time_diff == min(time_diff)) %>%
  ungroup() %>%
  select(..idx, ..idy)

mode <- "left"

join_func <- switch(mode,
                    inner = inner_join,
                    left = left_join,
                    right = right_join,
                    full = full_join,
                    semi = semi_join,
                    anti = anti_join
)

if (mode %in% c("inner", "left", "right", "full")) {
  ret <- x_i %>%
    join_func(pairs, by = "..idx") %>%
    join_func(y_i, by = c(by_user, "..idy" = "..idy")) %>%
    select(-..idx, -..idy)
} else if (mode %in% c("semi", "anti")) {
  ret <- x_i %>%
    join_func(pairs, by = "..idx") %>%
    select(-..idx)
}

gap_join <- function(x,
                       y,
                       by_time,
                       by_user,
                       mode = "inner",
                       type = "smallestgap") {

}

### within gap


as_seconds(20)

library(lubridate)  # has as.difftime

as_seconds(as.difftime(20, units = "hours"))
as_seconds(as.difftime(20, units = "days"))
as_seconds(as.difftime(20, units = "days"), sql = TRUE)

# Note that you have to use !! if you use this in SQL dplyr. Check this out:

library(dplyr)
library(datacampr)
tbl_main_courses() %>%
  select(created_at, updated_at) %>%
  mutate(time_diff = updated_at - created_at) %>%
  filter(time_diff < !!as_seconds(as.difftime(10, unit = "days"), sql = TRUE)) %>%
  count()

dt <- as.difftime(20, units = "hours")

pairs <- x_i %>%
  inner_join(y_i, by = user_xy) %>%
  filter(timestamp.x <= timestamp.y) %>%
  mutate(diff = difftime(timestamp.y, timestamp.x, unit = "secs")) %>%
  filter(diff < as_seconds(dt))

# these are the ids of the valid pairs
