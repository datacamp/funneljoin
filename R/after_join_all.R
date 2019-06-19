#' View result for each type of afterjoin
#'
#' @param x A tbl representing the first event to occur in the funnel.
#' @param y A tbl representing an event to occur in the funnel.
#' @param by_time A character vector to specify the time columns in x and y.
#'   This would typically be a datetime or a date column.
#'   These columns are used to filter for time y being after time x.
#' @param by_user A character vector to specify the user or identity columns in
#'   x and y.
#' @param mode The method used to join: "inner", "full", "anti", "semi",
#'   "right", "left".
#' @param ... any additional arguments
#' @importFrom magrittr %>%
#' @importFrom rlang :=
after_join_all <- function(x, y, by_user, by_time, mode = 'inner', ...){

  types <- c(
    'first-first', 'first-firstafter', 'lastbefore-firstafter',
    'any-firstafter', 'any-any'
  )

  by_type <- function(type){
    after_join(x, y, by_user = by_user,
               by_time = by_time, mode = mode, type = type) %>%
      dplyr::mutate(!!type := 'Y')
  }

  join_fun <- match.fun(paste0(mode, '_join'))

  all_types <- types %>%
    purrr::map(by_type)

  join_fun(x, y, by = by_user) %>%
    Reduce(dplyr::left_join, all_types, init = .) %>%
    dplyr::filter_at(dplyr::vars(dplyr::one_of(types)),
                     dplyr::any_vars(!is.na(.)))
}
