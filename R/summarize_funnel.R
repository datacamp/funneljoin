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


  ret <- x %>%
    dplyr::filter(dplyr::n() == 2,
           any(alternative.name == "control")) %>%
    dplyr::arrange(alternative.name != "control") %>%
    dplyr::do(broom::tidy(stats::prop.test(.$nb_conversions,
                                           .$nb_starts,
                                           conf.level = .9, ...))) %>%
    dplyr::transmute(control = estimate1,
              treatment = estimate2,
              p_value = p.value,
              conf.low,
              conf.high)

  if (ungroup) {
    ret <- ungroup(ret)
  }

  ret <- ret %>%
    dplyr::mutate(pct_change = (treatment - control) / control,
           pct_change_low = -conf.high / control,
           pct_change_high = -conf.low / control) %>%
    dplyr::select(-conf.low, -conf.high)

  ret
}

#' Summarize Left-joined table into conversion count
#'
#' @param x
#'
#' @return a table with three columns, `nb_starts`, `nb_conversions`, and `alternative.name`
#' @export
summarize_conversions <- function(x) {

  ret <- x %>%
    dplyr::count(alternative.name, !is.na(timestamp.y)) %>%
    dplyr::add_count(alternative.name, wt = n) %>%
    dplyr::rename(nb_starts = nn, nb_conversions = n) %>%
    dplyr::filter(`!is.na(timestamp.y)`) %>%
    dplyr::select(-`!is.na(timestamp.y)`)

  ret
}
