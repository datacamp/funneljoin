after_count <- function(x,
                        y,
                        by_time,
                        by_user,
                        mode = "inner",
                        type = "first",
                        max_gap = NULL,
                        min_gap = NULL,
                        gap_col = FALSE,
                        suffix = c(".x", ".y")) {

  user_xy <- dplyr::common_by(by_user, x, y)
  time_xy <- dplyr::common_by(by_time, x, y)
  new_type <- paste0(type, "-any")

  after_join_result <- after_join(x = x,
                                  y = y,
                                  by_time = by_time,
                                  by_user = by_user,
                                  mode = mode,
                                  type = new_type,
                                  min_gap = min_gap,
                                  max_gap = max_gap,
                                  gap_col = gap_col,
                                  suffix = suffix)

  if (time_xy$x == time_xy$y) {
    time_x <- paste0(time_xy$x, suffix[1])
    after_join_result %>%
      count(!!dplyr::sym((user_xy$x)),
            !!dplyr::sym((user_xy$y)),
            !!dplyr::sym((time_x)))
  }
  else {
    after_join_result %>%
      count(!!dplyr::sym((user_xy$x)),
            !!dplyr::sym((user_xy$y)),
            !!dplyr::sym((time_xy$x)))
  }
}

