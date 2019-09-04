#' Start a funnel
#'
#' @param tbl A table of different moments and timestamps
#' @param moment_type The first moment in the funnel
#' @param moment The name of the column with the moment_type
#' @param tstamp The name of the column with the timestamps of the moments
#' @param user The name of the column indicating the user who did the moment
#'
#' @export
#'
funnel_start <- function(tbl, moment_type, moment, tstamp, user) {

  if (!(moment_type %in% tbl[[dplyr::quo_name(dplyr::enquo(moment))]])) {
    stop(paste(moment_type, " is not in the moment column"))
  }

  md <- list(
    original_data = tbl,
    tstamp = dplyr::quo_name(dplyr::enquo(tstamp)),
    user = dplyr::quo_name(dplyr::enquo(user)),
    moment = dplyr::quo_name(dplyr::enquo(moment)),
    moment_sequence = moment_type,
    type_sequence = character(0)
  )

  attr(tbl, "funnel_metadata") <- md
  tbl <- tbl[tbl[[md$moment]] == moment_type, ]

  ret <- tbl %>%
    dplyr::select(-!!dplyr::sym(md$moment)) %>%
    dplyr::rename_at(dplyr::vars(-!!md$user), paste0, "_", moment_type)

  if (!inherits(.data, "tbl_lazy")) {
    ret <- ret %>%
      dplyr::select_if(~ any(!is.na(.)))
  }
  class(ret) <- c("tbl_funnel", class(ret))

  ret
}

#' Continue to funnel
#'
#' @param tbl A table of different moments and timestamps
#' @param moment_type The next moment in the funnel
#' @param type The type of after_join (e.g. "first-first", "any-any")
#' @param moment_types For \code{funnel_steps}, a character vector of
#' moment types, which are applied in order
#' @param name If you want a custom name instead of the moment_type; needed if the moment type is already in the sequence
#' @param optional Whether this step in the funnel should be optional. If so, the following step will also try joining
#' in a way that skips this step. Note that multiple optional steps in a row aren't supported.
#' @param ... Extra arguments passed on to \link{after_left_join}. For \code{funnel_steps}, these are passed on to \code{funnel_step}.
#' @export
#'
funnel_step <- function(tbl, moment_type, type, name = moment_type, optional = FALSE, ...) {
  md <- attr(tbl, "funnel_metadata")

  if (moment_type %in% md$moment_sequence && moment_type == name) {
    stop(paste(moment_type, "is already in the sequence; use the name argument to specify a custom name for the new moment."))
  }

  last_moment <- utils::tail(md$moment_sequence, 1)

  filtered <- md$original_data[md$original_data[[md$moment]] == moment_type, ]

  second_moment_data <- filtered %>%
    dplyr::select(-!!dplyr::sym(md$moment)) %>%
    dplyr::rename_at(dplyr::vars(-!!md$user), paste0, "_", name)

  tstamp_by <- stats::setNames(paste0(md$tstamp, "_", name),
                               paste0(md$tstamp, "_", last_moment))

  ret <- tbl %>%
    after_left_join(second_moment_data,
                    by_user = md$user,
                    by_time = tstamp_by,
                    type = type,
                    ...)

  last_optional <- length(md$optional_sequence) && utils::tail(md$optional_sequence, 1)

  if (last_optional) {
    if (optional) {
      stop("Multiple optional steps in a row is not yet supported")
    }

    # Try joining by the second to last step, which will take priority
    data_before_optional <- md$data_before_optional
    penultimate_moment <- tail(attr(data_before_optional, "funnel_metadata")$moment_sequence, 1)

    tstamp_current <- paste0(md$tstamp, "_", name)
    tstamp_penultimate <- paste0(md$tstamp, "_", penultimate_moment)
    tstamp_penultimate_by <- stats::setNames(tstamp_current, tstamp_penultimate)

    # If there are multiple user-tstamp ties for penultimate moment, doesn't work
    counts <- tbl %>%
      dplyr::count(!!sym(md$user), !!sym(tstamp_penultimate))

    if (any(counts$n > 1)) {
      stop("Can't use an optional step if there are duplicate ", md$user, ", ", tstamp_penultimate, " pairs in data")
    }

    ret_penultimate <- data_before_optional %>%
      after_left_join(second_moment_data,
                      by_user = md$user,
                      by_time = tstamp_penultimate_by,
                      type = type,
                      ...)

    # Be willing to use ret_penultimate's second step if it exists
    ret <- ret %>%
      right_join(ret_penultimate, by = c(md$user, tstamp_penultimate)) %>%
      mutate(!!tstamp_current := coalesce(!!rlang::sym(paste0(tstamp_current, ".y")),
                                         !!rlang::sym(paste0(tstamp_current, ".x")))) %>%
      select(-ends_with(".x"), -ends_with(".y"))

    md$data_before_optional <- NULL
  }

  # Append to the sequences
  md$moment_sequence <- c(md$moment_sequence, name)
  md$type_sequence <- c(md$type_sequence, type)
  md$optional_sequence <- c(md$optional_sequence, optional)

  if (optional) {
    md$data_before_optional <- tbl
  }

  attr(ret, "funnel_metadata") <- md

  ret
}

#' @export
#'
#' @rdname funnel_step
funnel_steps <- function(tbl, moment_types, type, ...) {
  for (s in moment_types) {
    tbl <- funnel_step(tbl, s, type, ...)
  }
  tbl
}

