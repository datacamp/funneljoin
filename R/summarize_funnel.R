#' Summarise after join funnel with proportion test
#'
#' @param x a data.frame
#' @param ...
#' @param ungroup whether the table needs to be ungrouped
#'
#' @return a data.frame with proportion test results
#' @export
summarize_prop_tests <- function(x, ..., ungroup = TRUE) {
  if (nrow(x) > 2 && is.null(dplyr::groups(x))) {
    stop("Need to group_by (probably by alternative.name column)")
  }
  if (!('alternative.name' %in% colnames(x))) {
    stop("Need a column called alternative.name")
  }

  prepared <- x %>%
    dplyr::filter(dplyr::n() == 2) %>%
    dplyr::arrange(alternative.name)

  alternatives <- sort(unique(prepared$alternative.name))

  if (length(alternatives) != 2) {
    stop("Must have exactly two alternatives")
  }

  ret <- prepared %>%
    dplyr::do(broom::tidy(stats::prop.test(.$nb_conversions,
                                           .$nb_starts,
                                           conf.level = .9, ...))) %>%
    dplyr::transmute(estimate1,
              estimate2,
              p_value = p.value,
              conf.low,
              conf.high)

  if (ungroup) {
    ret <- ungroup(ret)
  }

  vars <- c("estimate1", "estimate2")
  names(vars) <- alternatives

  ret <- ret %>%
    dplyr::mutate(pct_change = (estimate2 - estimate1) / estimate1,
           pct_change_low = -conf.high / estimate1,
           pct_change_high = -conf.low / estimate1) %>%
    dplyr::select(-conf.low, -conf.high) %>%
    dplyr::rename(!!vars)

  ret
}

#' Summarize Left-joined table into conversion count
#'
#' @param x a data.frame with one row per user
#' @param time_col_y the name of the second event time column
#'
#' @return a table with three columns, `nb_starts`, `nb_conversions`, and `alternative.name`
#' @export
summarize_conversions <- function(x, time_col_y = timestamp.y) {

  var_enq <- dplyr::enquo(time_col_y)

  if (inherits(x, "tbl_lazy")) {
    ret <- x %>%
      dplyr::group_by(alternative.name, add = TRUE) %>%
      dplyr::summarise(nb_starts = dplyr::n(),
                       nb_conversions = COUNT(!!var_enq))
  }

  else {
    ret <- x %>%
      dplyr::group_by(alternative.name, add = TRUE) %>%
      dplyr::summarise(nb_starts = dplyr::n(),
                       nb_conversions = sum(!is.na(!!var_enq)))
  }

  ret
}

