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
  check_missing_(data, .dots = lazyeval::lazy_dots(...),
                 ret_prop = ret_prop)
}

#' @describeIn check_missing SE version of check_missing.
#' @export
check_missing_ <- function(data, ..., .dots, ret_prop = TRUE) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::select_vars_(names(data), dots)

  if (ret_prop) {
    fun <- dplyr::funs_(quote(sum(is.na(.)) / n()))
  } else {
    fun <- dplyr::funs_(quote(sum(is.na(.))))
  }

  dplyr::summarise_at(data, .cols = vars, .funs = fun)
}
