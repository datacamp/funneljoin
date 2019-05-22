#' Summarise after join funnel with proportion test
#'
#' @param x a data.frame with columns nb_conversions and nb_starts
#' @param alternative_name the name of the column indicating the experiment group
#' @param ... any additional arguments
#' @param ungroup whether the table needs to be ungrouped
#'
#' @return a data.frame with proportion test results
#' @export
summarize_prop_tests <- function(x, alternative_name = alternative.name, ..., ungroup = TRUE) {
  if (nrow(x) > 2 && is.null(dplyr::groups(x))) {
    stop("Need to group_by (probably by alternative_name column)")
  }
  if (inherits(x, "tbl_lazy")) {
    stop("Needs to be a local (not remote) table")
  }

  var_enq_alternative <- dplyr::enquo(alternative_name)

  prepared <- x %>%
    dplyr::filter(dplyr::n() == 2) %>%
    dplyr::arrange(!!var_enq_alternative)

  alternatives <- sort(unique(dplyr::pull(prepared, !!var_enq_alternative)))

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
#' @param x A tbl with one row per user
#' @param converted The name of the column representing whether the user converted
#' (treated as FALSE if NA or FALSE, otherwise TRUE)
#'
#' @return A table with columns for your groups, along with `nb_starts` and `nb_conversions`
#' @export
summarize_conversions <- function(x, converted) {

  var_converted <- dplyr::enquo(converted)
  if (inherits(x, "tbl_lazy")) {
    first_value <- x %>%
      head(1) %>%
      pull(!!var_converted)

    if (is.logical(first_value)) {
      ret <- x %>%
        dplyr::summarise(nb_starts = n(),
                         nb_conversions = sum(ifelse(!!var_converted, 1, 0)))

    }
    else {
      ret <- x %>%
        dplyr::summarise(nb_starts = n(),
                         nb_conversions = COUNT(!!var_converted))
    }
  }

  else {
    ret <- x %>%
      dplyr::summarise(nb_starts = dplyr::n(),
                       nb_conversions = ifelse(is.logical(!!var_converted),
                                                sum(!!var_converted),
                                                sum(!is.na(!!var_converted))))
  }

  ret
}
