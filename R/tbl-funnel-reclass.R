#' Copy class and attributes from the original version of an object to a modified version.
#'
#' Copied over from https://github.com/tidyverse/dplyr/issues/719
#' @export
#' @param x The original object, which has a class/attributes to copy
#' @param result The modified object, which is / might be missing the
#'   class/attributes.
#'
#' @import dplyr
#'
#' @return \code{result}, now with class/attributes restored.
reclass <- function(x, result) {
  UseMethod('reclass')
}

reclass.tbl_funnel <- function(x, result) {
  class(result) <- unique(c(class(x)[[1]], class(result)))
  attr(result, class(x)[[1]]) <- attr(x, class(x)[[1]])
  attr(result, 'funnel_metadata') <- attr(x, 'funnel_metadata')
  result
}


#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @rdname funnel-s3
#' @export
filter.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
select.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
arrange.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
mutate.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
group_by.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
summarise.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
inner_join.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
left_join.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
right_join.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
semi_join.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
anti_join.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
rename.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

#' @rdname funnel-s3
#' @export
transmute.tbl_funnel <- function(.data, ...) {
  reclass(.data, NextMethod())
}

