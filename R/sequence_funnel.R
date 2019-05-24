tbl_views_snowplow_moments() %>%
  filter(tstamp > "2019-05-22") %>%
  funnel_start("registration", moment_type, tstamp, user_id)
  funnel_step("exercise_start", type = "first-firstafter")
  funnel_step("project_start", type = "first-firstafter") %>%
  funnel_step("subscription", type = "first-firstafter") %>%
  funnel_end()

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
  attr(tbl, "original_data") <- tbl
  attr(tbl, "tstamp") <- dplyr::enquo(tstamp)
  attr(tbl, "user") <- dplyr::enquo(user)
  attr(tbl, "event") <- dplyr::enquo(event)
  attr(tbl, "event_sequence") <- event_type

  quo_event <- attr(tbl, "event")

  tbl %>%
    dplyr::filter(!!quo_event == event_type) %>%
    dplyr::rename_at(vars(-!!enquo(user)), paste0, "_", event_type)

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

  quo_event <- attr(tbl, "event")

  second_event_data <- attr(tbl, "original_data") %>%
    dplyr::filter(!!quo_event == event_type)

  tbl %>%
    after_left_join(second_event_data,
                    by_user = quo_name(attr(tbl, "user")),
                    by_time = quo_name(attr(tbl, "tstamp")),
                    type = type)

}

#' Title
#'
#' @return
#' @export
#'
#' @examples
funnel_end <- function(tbl) {

}
