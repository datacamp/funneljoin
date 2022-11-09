#' Join tables based on one event happening after another
#'
#' Join two tables based on observations in one table happening after
#' observations in the other. Each table must have a user_id column,
#' which must always match for two observations to be joined,
#' and a time column, which must be greater in \code{y} than in \code{x} for
#' the two to be joined.
#' Supports all types of dplyr joins (inner, left, anti, etc.) and requires a
#' type argument to specify which observations in a funnel get kept
#' (see details for supported types).
#'
#' @param x A tbl representing the first event to occur in the funnel.
#' @param y A tbl representing an event to occur in the funnel.
#' @param by_time A character vector to specify the time columns in x and y.
#'   This would typically be a datetime or a date column. These columns are used to
#'   filter for time y being after time x.
#' @param by_user A character vector to specify the user or identity columns in
#'   x and y.
#' @param mode The method used to join: "inner", "full", "anti", "semi",
#'   "right", "left". Each also has its own function, such as
#'   \code{after_inner_join}.
#' @param type The type of funnel used to distinguish between event pairs, such
#'   as "first-first", "last-first", or "any-firstafter". See details for more.
#' @param max_gap Optional: the maximum gap allowed between events. Can be a
#'   integer representing the number of seconds or a difftime object, such as
#'   \code{as.difftime(2, units = "hours")}.
#' @param min_gap Optional: the maximum gap allowed between events. Can be a
#'   integer representing the number of seconds or a difftime object, such as
#'   \code{as.difftime(2, units = "hours")}.
#' @param gap_col Whether to include a numeric column, \code{gap},
#'   with the time difference in seconds between the events.
#' @param suffix If there are non-joined duplicate variables in x and y,
#' these suffixes will be added to the output to disambiguate them.
#' Should be a character vector of length 2.
#' @importFrom magrittr %>%
#'
#' @details
#'
#' \code{type} can be any combination of \code{first}, \code{last}, \code{any}, \code{lastbefore}, \code{firstwithin} with \code{first}, \code{last}, \code{any},  \code{firstafter}. Some common ones you may use include:
#' \describe{
#'   \item{first-first}{Take the earliest x and y for each user \bold{before} joining. For example, you want the first time someone entered an experiment, followed by the first time someone \bold{ever} registered. If they registered, entered the experiment, and registered again, you do not want to include that person.}
#'   \item{first-firstafter}{Take the first x, then the first y after that. For example, you want when someone first entered an experiment and the first course they started afterwards. You don't care if they started courses before entering the experiment. }
#'   \item{lastbefore-firstafter}{First x that's followed by a y before the next x. For example, in last click paid ad attribution, you want the last time someone clicked an ad before the first subscription they did afterward.}
#'   \item{any-firstafter}{Take all Xs followed by the first Y after it. For example, you want all the times someone visited a homepage and their first product page they visited afterwards.}
#'   \item{any-any}{Take all Xs followed by all Ys. For example, you want all the times someone visited a homepage and \bold{all} the product pages they saw afterward.}
#'   }
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' landed <- tribble(
#'   ~user_id, ~timestamp,
#'   1, "2018-07-01",
#'   2, "2018-07-01",
#'   2, "2018-07-01",
#'   3, "2018-07-02",
#'   4, "2018-07-01",
#'   4, "2018-07-04",
#'   5, "2018-07-10",
#'   5, "2018-07-12",
#'   6, "2018-07-07",
#'   6, "2018-07-08"
#' ) %>%
#'   mutate(timestamp = as.Date(timestamp))
#'
#' registered <- tribble(
#'   ~user_id, ~timestamp,
#'   1, "2018-07-02",
#'   3, "2018-07-02",
#'   4, "2018-06-10",
#'   4, "2018-07-02",
#'   5, "2018-07-11",
#'   6, "2018-07-10",
#'   6, "2018-07-11",
#'   7, "2018-07-07"
#' ) %>%
#'  mutate(timestamp = as.Date(timestamp))
#'
#' after_inner_join(landed, registered, by_user = "user_id",
#'            by_time = "timestamp", type = "first-first")
#'
#' # You can use different methods of joining:
#' after_left_join(landed, registered, by_user = "user_id",
#'            by_time = "timestamp", type = "first-first")
#'
#' after_anti_join(landed, registered, by_user = "user_id",
#'            by_time = "timestamp", type = "any-any")
#'
after_join <- function(x,
                       y,
                       by_time,
                       by_user,
                       mode = "inner",
                       type = "first-first",
                       max_gap = NULL,
                       min_gap = NULL,
                       gap_col = FALSE,
                       suffix = c(".x", ".y")) {

  if (!is.null(attr(x, "after_join")) & inherits(x, "tbl_lazy")) {
    stop("You can not do multiple after joins in a row remotely. Please pull your data locally.")
  }

  types <- strsplit(type, '\\-')[[1]]

  if (length(types) != 2) {
    stop("type argument only supports pairs")
  }

  type_x <- match.arg(types[1], c("first", "last", "any", "lastbefore", "firstwithin"))
  type_y <- match.arg(types[2], c("first", "last", "any", "firstafter"))

  if (length(by_user) > 1) {
    stop("Joining on multiple user columns is not supported. Check the by_user argument.")
  }

  if (length(by_time) > 1) {
    stop("Joining on multiple time columns is not supported. Check the by_time argument.")
  }

  user_xy <- dplyr::common_by(by_user, x, y)
  time_xy <- dplyr::common_by(by_time, x, y)

  x_i <- x %>%
    dplyr::arrange(!!dplyr::sym(user_xy$x), !!dplyr::sym(time_xy$x)) %>%
    dplyr::mutate(..idx = row_number())

  y_i <- y %>%
    dplyr::arrange(!!dplyr::sym(user_xy$y), !!dplyr::sym(time_xy$y)) %>%
    dplyr::mutate(..idy = row_number())

  if (type_x %in% c("first", "last")) {
    x_i <- x_i %>%
      distinct_events(time_col = time_xy$x,
                      user_col = user_xy$x,
                      type = type_x)
  }

  if (type_y %in% c("first", "last")) {
    y_i <- y_i %>%
      distinct_events(time_col = time_xy$y,
                      user_col = user_xy$y,
                      type = type_y)
  }

  # Handle the case when columns with the same name are appended with .x & .y
  if (time_xy$x == time_xy$y || time_xy$x %in% colnames(y_i) || time_xy$y %in% colnames(x_i)) {
    time_xy <- list(x = paste0(time_xy$x, ".x"),
                    y = paste0(time_xy$y, ".y"))
  }

  # Get all the matching rows
  pairs <- x_i %>%
    dplyr::inner_join(y_i, by = user_xy) %>%
    dplyr::filter(!!dplyr::sym(time_xy$x) <= !!dplyr::sym(time_xy$y))

  if (!is.null(max_gap) || !is.null(min_gap)) {
    pairs <- filter_in_gap(pairs = pairs,
                           max_gap = max_gap,
                           min_gap = min_gap,
                           time_xy = time_xy,
                           user_xy = user_xy)
  }

  if (type_x == "firstwithin") {
    pairs <- pairs %>%
      distinct_events(time_col = time_xy$x,
                      user_col = "..idy",
                      type = "first")
    }

  if (type_y == "firstafter") {
    pairs <- pairs %>%
      distinct_events(time_col = time_xy$y,
                      user_col = "..idx",
                      type = "first")
  }

  if (type_x == "lastbefore") {
    pairs <- pairs %>%
      distinct_events(time_col = time_xy$x,
                      user_col = "..idy",
                      type = "last")
  }

  if (gap_col) {
    if (inherits(pairs, "tbl_BigQueryConnection")) {
      time_difference <- dplyr::sql(glue::glue('DATETIME_DIFF(`{ time_xy$y }`,
                                               `{ time_xy$x }`,
                                               SECOND)'))

      pairs <- pairs %>%
        dplyr::mutate(gap = time_difference) %>%
        dplyr::select(..idx, ..idy, gap)
    }
    else if (inherits(pairs, "tbl_lazy")) {
      time_difference <- dplyr::sql(glue::glue('DATEDIFF("seconds",
                                               "{ time_xy$x }",
                                               "{ time_xy$y }")::integer'))

      pairs <- pairs %>%
        dplyr::mutate(gap = time_difference) %>%
        dplyr::select(..idx, ..idy, gap)
    }
    else {
      pairs <- pairs %>%
        dplyr::mutate(gap = as.numeric(difftime(!!dplyr::sym(time_xy$y),
                                                 !!dplyr::sym(time_xy$x), units = "secs"))) %>%
        dplyr::select(..idx, ..idy, gap)
    }
  } else {
    pairs <- pairs %>%
      dplyr::select(..idx, ..idy)
  }

  join_func <- switch(mode,
                      inner = dplyr::inner_join,
                      left = dplyr::left_join,
                      right = dplyr::right_join,
                      full = dplyr::full_join,
                      semi = dplyr::semi_join,
                      anti = dplyr::anti_join
  )

  if (is.null(join_func)) {
    stop("Unknown joining mode: ", mode)
  }

  if (mode %in% c("inner", "left", "right", "full")) {
    ret <- x_i %>%
      join_func(pairs, by = "..idx") %>%
      join_func(y_i, by = c(by_user, "..idy" = "..idy"), suffix = suffix) %>%
      dplyr::select(-..idx, -..idy)
  } else if (mode %in% c("semi", "anti")) {
    ret <- x_i %>%
      join_func(pairs, by = "..idx") %>%
      dplyr::select(-..idx)
  }

  attr(ret, "after_join") <- 1
  ret
}

#' @rdname after_join
#' @export
after_inner_join <- function(x, y, by_time, by_user, type, max_gap = NULL,  min_gap = NULL, gap_col = FALSE, suffix = c(".x", ".y")) {
  after_join(x, y, by_time, by_user,
             mode = "inner", type = type, max_gap = max_gap, min_gap = min_gap, gap_col = gap_col, suffix = suffix)
}

#' @rdname after_join
#' @export
after_left_join <- function(x, y, by_time, by_user, type, max_gap = NULL, min_gap = NULL, gap_col = FALSE, suffix = c(".x", ".y")) {
  after_join(x, y, by_time, by_user,
             mode = "left", type = type, max_gap = max_gap, min_gap = min_gap, gap_col = gap_col, suffix = suffix)
}

#' @rdname after_join
#' @export
after_right_join <- function(x, y, by_time, by_user, type, max_gap = NULL, min_gap = NULL, gap_col = FALSE, suffix = c(".x", ".y")) {
  after_join(x, y, by_time, by_user,
             mode = "right", type = type, max_gap = max_gap, min_gap = min_gap, gap_col = gap_col, suffix = suffix)
}

#' @rdname after_join
#' @export
after_full_join <- function(x, y, by_time, by_user, type, max_gap = NULL, min_gap = NULL, gap_col = FALSE, suffix = c(".x", ".y")) {
  after_join(x, y, by_time, by_user,
             mode = "full", type = type, max_gap = max_gap, min_gap = min_gap, gap_col = gap_col, suffix = suffix)
}

#' @rdname after_join
#' @export
after_anti_join <- function(x, y, by_time, by_user, type, max_gap = NULL, min_gap = NULL, gap_col = FALSE, suffix = c(".x", ".y")) {
  after_join(x, y, by_time, by_user,
             mode = "anti", type = type, max_gap = max_gap, min_gap = min_gap, gap_col = gap_col, suffix = suffix)
}

#' @rdname after_join
#' @export
after_semi_join <- function(x, y, by_time, by_user, type, max_gap = NULL, min_gap = NULL,
                            gap_col = FALSE, suffix = c(".x", ".y")) {
  after_join(x, y, by_time, by_user,
             mode = "semi", type = type, max_gap = max_gap, min_gap = min_gap, gap_col = gap_col, suffix = suffix)
}
