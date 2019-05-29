if (FALSE) {
  library(dplyr)
  library(datacampr)
  tbl_views_snowplow_moments() %>%
    filter(tstamp > "2019-05-22") %>%
    funnel_start("registration", moment_type, tstamp, user_id) %>%
    funnel_step("exercise_start", type = "first-firstafter")


  funnel_step("project_start", type = "first-firstafter") %>%
    funnel_step("subscription", type = "first-firstafter")
}

#' Start a funnel
#'
#' @param tbl A table
#' @param event_type The first event in the funnel
#' @param event The name of the column with the event_type
#' @param tstamp The name of the column with the timestamps of the events
#' @param user The name of the column indicating the user who did the event
#'
#' @return
#' @export
#'
#' @examples
funnel_start <- function(tbl, event_type, event, tstamp, user) {
  md <- list(
    original_data = tbl,
    tstamp = dplyr::quo_name(dplyr::enquo(tstamp)),
    user = dplyr::quo_name(dplyr::enquo(user)),
    event = dplyr::quo_name(dplyr::enquo(event)),
    event_sequence = event_type
  )

  attr(tbl, "funnel_metadata") <- md

  tbl %>%
    dplyr::filter(!!dplyr::sym(md$event) == event_type) %>%
    dplyr::rename_at(vars(-!!md$user), paste0, "_", event_type)
}

#' Title
#'
#' @param event_type
#' @param type
#'
#' @return
#' @export
#'
#' @examples
funnel_step <- function(tbl, event_type, type) {
  md <- attr(tbl, "funnel_metadata")
  last_event <- utils::tail(md$event_sequence, 1)
  md$event_sequence <- c(tbl$event_sequence, event_type)

  second_event_data <- md$original_data %>%
    dplyr::filter(!!dplyr::sym(md$event) == event_type) %>%
    dplyr::rename_at(vars(-!!md$user), paste0, "_", event_type)

  tstamp_by <- stats::setNames(paste0(md$tstamp, "_", event_type),
                               paste0(md$tstamp, "_", last_event))

  ret <- tbl %>%
    after_left_join(second_event_data,
                    by_user = md$user,
                    by_time = tstamp_by,
                    type = type)

  attr(ret, "funnel_metadata") <- md

  ret
}
