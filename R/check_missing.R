#' Check missing rate in variables.
#'
#' Check missing (\code{NA}) proportion or counts of variables. This function
#' works like \code{\link[dplyr]{summarize_at}} where the missing rate or
#' count for the selected columns are returned.
#'
#'
#' @inheritParams common_params
#' @inheritParams dplyr::select
#' @param ret_prop Whether to return the rate of missing (default) or counts.
#'
#' @author Min Ma
#' @export
check_missing <- function(data, ..., ret_prop = TRUE) {
  UseMethod("check_missing")
}

#' @describeIn check_missing S3 method for data.frame.
#' @export
check_missing.data.frame <- function(data, ..., ret_prop = TRUE) {
  # main method for NSE function
  dots <- quos(...)

  if (length(dots) == 0) {
    stop("Please specify unquoted column names, tidy select helpers supported.")
  }

  if (ret_prop) {
    fun <- dplyr::funs(sum(is.na(.)) / n())
  } else {
    fun <- dplyr::funs(sum(is.na(.)))
  }

  dplyr::summarise_at(data, .vars = dplyr::vars(UQS(dots)), .funs = fun)
}

#' @describeIn check_missing default method for new NSE generic that dispatches
#' to the NSE generic for backward compatibility.
#' @export
check_missing.default <- function(data, ...) {
  check_missing_(.data, .dots = compat_as_lazy_dots(...))
}

#' @export
check_missing_ <- function(ata, ..., .dots = list(),
                           ret_prop = TRUE) {
  # to provide backward compatibility to the old SE function
  UseMethod("check_missing_")
}

#' @export
check_missing_.data.frame <- function(data, ..., .dots = list(),
                                      ret_prop = TRUE) {
  dots <- compat_lazy_dots(.dots, rlang::caller_env(), ...)
  check_missing(data, UQS(dots), ret_prop = ret_prop)
}
