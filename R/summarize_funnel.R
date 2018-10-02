summarize_prop_tests <- function(x, ..., ungroup = TRUE) {
  if (nrow(x) > 2 && is.null(dplyr::groups(x))) {
    stop("Need to group_by (probably by name column)")
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
