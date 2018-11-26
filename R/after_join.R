#' Join tables based on one event happening after another
#'
#' @param x A tbl that is the first event to occur in the funnel.
#' @param y A tbl that is the following event to occur in the funnel.
#' @param by_time A character vector to specify the time columns in x and y.
#' Must be a single column in each tbl. Note that this column is used to filter for time y >= time x.
#' @param by_user A character vector to specify the user or identity columns in x and y.
#' Must be a single column in each tbl.
#' @param mode The method used to join: "inner", "full", "anti", "semi", "right", "left"
#' @param type The type of funnel used to distinguish between event pairs,
#' such as "first-first", "last-first", "any-firstafter". See details for more.
#' @param max_gap optional: the maximum gap allowed between events. Can be a integer representing the number of seconds or a difftime object.
#' @param gap TRUE or FALSE for whether you want to return a column, .gap, that's the time difference in seconds between the events.
#' @importFrom magrittr %>%
#' @details TODO
#'
#'
#' @return A tbl
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
#' after_join(landed, registered, by_user = "user_id",
#'            by_time = "timestamp", mode = "inner", type = "first-first")
#'
#' after_join(landed, registered, by_user = "user_id",
#'            by_time = "timestamp", mode = "inner", type = "any-firstafter")
#'
#' after_join(landed, registered, by_user = "user_id",
#'            by_time = "timestamp", mode = "inner", type = "any-any")
#'
#' # You can change mode to control the method of joining:
#' after_join(landed, registered, by_user = "user_id",
#'            by_time = "timestamp", mode = "left", type = "first-first")
#'
#' after_join(landed, registered, by_user = "user_id",
#'            by_time = "timestamp", mode = "right", type = "any-firstafter")
#'
#' after_join(landed, registered, by_user = "user_id",
#'            by_time = "timestamp", mode = "anti", type = "any-any")
#'
after_join <- function(x,
                       y,
                       by_time,
                       by_user,
                       mode = "inner",
                       type = "first-first",
                       max_gap = NULL,
                       gap = FALSE) {

  types <- stringr::str_split(type, '\\-')[[1]]

  if (type == "lastbefore-firstafter") {
    types[1] = "any"
    types[2] = "any"
  }

  if (length(types) != 2) {
    stop("type argument only supports pairs")
  }

  type_x <- match.arg(types[1], c("first", "last", "any"))
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
    dplyr::mutate(..idx = row_number())

  y_i <- y %>%
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
  if (time_xy$x == time_xy$y) {
    time_xy <- list(x = paste0(time_xy$x, ".x"),
                    y = paste0(time_xy$y, ".y"))
  }

  # Get all the matching rows
  pairs <- x_i %>%
    dplyr::inner_join(y_i, by = user_xy) %>%
    dplyr::filter(!!dplyr::sym(time_xy$x) <= !!dplyr::sym(time_xy$y))

  if (type_y == "firstafter") {
    pairs <- pairs %>%
      distinct_events(time_col = time_xy$y,
                      user_col = "..idx",
                      type = "first")
  }

  if (type == "lastbefore-firstafter") {
    # pick earliest y, then last x before it
    pairs <- pairs %>%
      dplyr::group_by(!!dplyr::sym(user_xy$x)) %>%
      dplyr::filter(!!dplyr::sym(time_xy$y) == min(!!dplyr::sym(time_xy$y),
                                            na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      distinct_events(user_col = user_xy$x,
                      time_col = time_xy$x,
                      type = "last")
  }

  if (!is.null(max_gap)) {
    pairs <- filter_within_gap(pairs = pairs,
                               max_gap = max_gap,
                               time_xy = time_xy,
                               user_xy = user_xy)
  }

  if (gap) {
    if (inherits(pairs, "tbl_lazy")) {
      time_difference <- dplyr::sql(glue::glue('DATEDIFF("seconds",
                                               "{ time_xy$x }",
                                               "{ time_xy$y }")::integer'))

      pairs <- pairs %>%
        dplyr::mutate(.gap = time_difference) %>%
        dplyr::select(..idx, ..idy, .gap)
    }
    else {
      pairs <- pairs %>%
        dplyr::mutate(.gap = difftime(!!dplyr::sym(time_xy$y),
                                      !!dplyr::sym(time_xy$x), "secs")) %>%
        dplyr::select(..idx, ..idy, .gap)
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
      join_func(y_i, by = c(by_user, "..idy" = "..idy")) %>%
      dplyr::select(-..idx, -..idy)
  } else if (mode %in% c("semi", "anti")) {
    ret <- x_i %>%
      join_func(pairs, by = "..idx") %>%
      dplyr::select(-..idx)
  }

  ret
}

#' @rdname after_join
#' @export
after_inner_join <- function(x, y, by_time, by_user, type, max_gap = NULL) {
  after_join(x, y, by_time, by_user,
             mode = "inner", type = type, max_gap = max_gap)
}

#' @rdname after_join
#' @export
after_left_join <- function(x, y, by_time, by_user, type, max_gap = NULL) {
  after_join(x, y, by_time, by_user,
             mode = "left", type = type, max_gap = max_gap)
}

#' @rdname after_join
#' @export
after_right_join <- function(x, y, by_time, by_user, type, max_gap = NULL) {
  after_join(x, y, by_time, by_user,
             mode = "right", type = type, max_gap = max_gap)
}

#' @rdname after_join
#' @export
after_full_join <- function(x, y, by_time, by_user, type, max_gap = NULL) {
  after_join(x, y, by_time, by_user,
             mode = "full", type = type, max_gap = max_gap)
}

#' @rdname after_join
#' @export
after_anti_join <- function(x, y, by_time, by_user, type, max_gap = NULL) {
  after_join(x, y, by_time, by_user,
             mode = "anti", type = type, max_gap = max_gap)
}

#' @rdname after_join
#' @export
after_semi_join <- function(x, y, by_time, by_user, type, max_gap = NULL) {
  after_join(x, y, by_time, by_user,
             mode = "semi", type = type, max_gap = max_gap)
}
