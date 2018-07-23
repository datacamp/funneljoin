
# Types of joins. Will always return at most one row per user
#
# * first-first: You can take earliest x and y for each user before the join (entered an experiment, then registered)
# * first-firstafter: Take the first x, then the first y after that (entered an experiment, then started a course)
# * lastbefore-firstafter: First x that's followed by a y before the next x (last click paid ad attribution)
# * last-
# * smallestgap: Smallest x-y gap (in tie, earliest).
#
# Some also have multiple rows per user:
#
# * withingap: Gap is less than X (all ad clicks followed by a course start within an hour), smallest gap for each X
# * every-firstafter: All Xs followed by a Y


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
  6, "2018-07-07"
)

registered <- tribble(
  ~user_id, ~timestamp,
  1, "2018-07-02",
  3, "2018-07-02",
  4, "2018-06-10",
  4, "2018-07-02",
  5, "2018-07-11",
  6, "2018-07-10",
  7, "2018-07-07"
)

after_join <- function(x,
                       y,
                       by_time,
                       by_user,
                       mode = "inner",
                       criterion = "first-first") {

  if (length(by_user) > 1) {
    stop("Joining on multiple user columns is not supported. Check the by_user argument.")
  }

  if (length(by_time) > 1) {
    stop("Joining on multiple time columns is not supported. Check the by_time argument.")
  }

  user_xy <- dplyr:::common_by(by_user, x, y)
  time_xy <- dplyr:::common_by(by_time, x, y)



  x_distinct <- x %>%
    arrange(!!sym(time_xy$x)) %>%
    distinct(!!sym(user_xy$x), .keep_all = T)

  y_distinct <- y %>%
    arrange(!!sym(time_xy$y)) %>%
    distinct(!!sym(user_xy$y), .keep_all = T)

  # Handle the case where columns with the same name are appended with .x & .y
  if (time_xy$x == time_xy$y) {
    time_xy <- list(x = paste0(time_xy$x, ".x"),
                    y = paste0(time_xy$y, ".y"))
  }

  x_distinct %>%
    inner_join(y_distinct, by = user_xy) %>%
    filter(!!sym(time_xy$x) <= !!sym(time_xy$y))
}

after_join(landed, registered, by_user = "user_id", by_time = c("timestamp" = "timestamp"))
