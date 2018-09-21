summarize_prop_tests <- function(x, ..., ungroup = TRUE) {
  if (nrow(x) > 2 && is.null(groups(x))) {
    stop("Need to group_by (probably by name column)")
  }
  if (!('alternative.name' %in% colnames(x))) {
    stop("Need a column called alternative.name")
  }


  ret <- x %>%
    filter(n() == 2,
           any(alternative.name == "control")) %>%
    arrange(alternative.name != "control") %>%
    do(broom::tidy(prop.test(.$nb_conversions, .$nb_starts, conf.level = .9, ...))) %>%
    transmute(control = estimate1,
              treatment = estimate2,
              p_value = p.value,
              conf.low,
              conf.high)

  if (ungroup) {
    ret <- ungroup(ret)
  }

  ret <- ret %>%
    mutate(pct_change = (treatment - control) / control,
           pct_change_low = -conf.high / control,
           pct_change_high = -conf.low / control) %>%
    select(-conf.low, -conf.high)

  ret
}
