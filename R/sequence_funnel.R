#' Start a funnel
#'
#' @param tbl A table of different events and timestamps
#' @param event_type The first event in the funnel
#' @param event The name of the column with the event_type
#' @param tstamp The name of the column with the timestamps of the events
#' @param user The name of the column indicating the user who did the event
#'
#' @export
#'
funnel_start <- function(tbl, event_type, event, tstamp, user) {
  md <- list(
    original_data = tbl,
    tstamp = dplyr::quo_name(dplyr::enquo(tstamp)),
    user = dplyr::quo_name(dplyr::enquo(user)),
    event = dplyr::quo_name(dplyr::enquo(event)),
    event_sequence = event_type
  )

  attr(tbl, "funnel_metadata") <- md

  ret <- tbl %>%
    dplyr::filter(!!dplyr::sym(md$event) == event_type) %>%
    dplyr::select(-!!dplyr::sym(md$event)) %>%
    dplyr::rename_at(dplyr::vars(-!!md$user), paste0, "_", event_type)

  class(ret) <- c("tbl_funnel", class(ret))

  ret
}

#' Continue to funnel
#'
#' @param tbl A table of different events and timestamps
#' @param event_type The next event in the funnel
#' @param type The type of after_join (e.g. "first-first", "any-any")
#' @param ... Extra arguments passed on to \link{after_left_join}
#' @export
#'
funnel_step <- function(tbl, event_type, type, ...) {
  md <- attr(tbl, "funnel_metadata")
  last_event <- utils::tail(md$event_sequence, 1)
  md$event_sequence <- c(md$event_sequence, event_type)

  second_event_data <- md$original_data %>%
    dplyr::filter(!!dplyr::sym(md$event) == event_type) %>%
    dplyr::select(-!!dplyr::sym(md$event)) %>%
    dplyr::rename_at(dplyr::vars(-!!md$user), paste0, "_", event_type)

  tstamp_by <- stats::setNames(paste0(md$tstamp, "_", event_type),
                               paste0(md$tstamp, "_", last_event))

  ret <- tbl %>%
    after_left_join(second_event_data,
                    by_user = md$user,
                    by_time = tstamp_by,
                    type = type,
                    ...)

  attr(ret, "funnel_metadata") <- md

  ret
}
