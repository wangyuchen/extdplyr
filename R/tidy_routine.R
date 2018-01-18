#' Reshape data into a tidy format.
#'
#'
#'
#' @inheritParams tidyr::extract
#' @param into Character vector of variable names to create. \code{into} must
#' contain the value of \code{spread} and match the matching groups in
#' \code{regex}.
#' @param spread The variable used to spread, the default is \code{"var"}. If
#' \code{spread} is not one of the \code{into} variables,
#' \code{\link[tidyr]{spread}} will not be called.
#'
#' @example examples/tidy_routine_ex.R
#'
#' @export
tidy_routine <- function(data, into, regex = "([[:alnum:]]+)", spread = "var") {
  UseMethod("tidy_routine")
}

#' @describeIn tidy_routine base method that deals with the calculation.
#' @export
tidy_routine.data.frame <- function(data, into, regex = "([[:alnum:]]+)",
                                    spread = "var") {
  # Check length(into) == match groups
  # Check key and value are OK names

  if (!(spread %in% into)) {
    stop(paste("'spread' must be an element of 'into'."))
  }

  data %>%
    # gather cols that match regex into key and value
    tidyr::gather(key = "key", value = "value", dplyr::matches(regex)) %>%
    # extract using the same regex (old variable names)
    # keep into, remove all old ones
    tidyr::extract(col = "key", into = into, regex = regex,
                   remove = TRUE, convert = TRUE) %>%
    dplyr::mutate_at(into, dplyr::funs(factor(., unique(.)))) %>%
    tidyr::spread(key = spread, value = "value") %>%
    dplyr::mutate_at(into[into != spread], as.character)
}
