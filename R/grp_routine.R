#' Mutate a character/factor based on conditions.
#'
#' \code{grp_routine} functions like a serious of nested \code{ifelse} where
#' a series of conditions are evaluated and different values are assigned based
#' on those conditions.
#'
#'
#' @inheritParams common_params
#' @param ... Specification of group assignment. Use named conditions, like
#' \code{top2 = x > 5}.
#' @inheritParams ind_to_char
#'
#' @export
#' @example /examples/grp_routine_ex.R
grp_routine <- function(data, col, ..., ret_factor = FALSE,
                        na_as_false = FALSE) {
  col <- col_name(substitute(col))
  grp_routine_(data, col, .dots = lazyeval::lazy_dots(...),
                  ret_factor = ret_factor, na_as_false = na_as_false)
}

#' @describeIn grp_routine SE version of grp_routine.
#' @export
grp_routine_ <- function(data, col, ..., .dots, ret_factor = FALSE,
                         na_as_false = FALSE) {
  conds <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  data %>%
    dplyr::mutate_(.dots = conds) %>%
    ind_to_char_(col, names(conds), ret_factor = ret_factor,
                 remove = TRUE,
                 na_as_false = na_as_false)
}




#' Convert indicator data.frame to character/factor.
#'
#' This is the reverse operation of using \code{\link[stats]{model.matrix}} on a
#' factor. \code{ind_to_char} works like \code{tidyr::unite}, it combines
#' multiple indicator columns into one character/factor column and add it to
#' the data.
#'
#' @inheritParams common_params
#' @inheritParams tidyr::unite_
#' @param ... Specification of indicator columns. Use bare variable names.
#' Select all variables between \code{x} and \code{z} with \code{x:z}. For more
#' options, see the \code{\link[dplyr]{select}} documentation.
#' @param ret_factor Whether to convert the column into factor.
#' exhaustive.
#'
#' @example examples/ind_to_char_ex.R
#'
#' @export
ind_to_char <- function(data, col, ..., ret_factor = FALSE, remove = TRUE,
                        na_as_false = FALSE) {
  col <- col_name(substitute(col))
  from <- dplyr::select_vars(colnames(data), ...)
  ind_to_char_(data, col, from, ret_factor = ret_factor, remove = remove,
               na_as_false = na_as_false)
}


#' @describeIn ind_to_char SE version of \code{ind_to_char}.
#' @export
ind_to_char_ <- function(data, col, from, ret_factor = FALSE, remove = TRUE,
                         na_as_false = FALSE) {

  ind_df <- data[from]
  ind_df[] <- lapply(ind_df, as_indicator, convert_na = na_as_false)

  rs <- rowSums(ind_df)
  all_na <- apply(ind_df, 1, function(x) all(is.na(x)))

  if (any(rs > 1, na.rm = TRUE)) {
    # Have to stop here, because don't know which one to take
    stop("Indicators are not mutually exclusive, check overlaps.")
  }

  if (any(is.na(rs) & !all_na)) {
    # This checks any partial NA in one row. All NA is allowed.
    # If na_as_false = TRUE, all NAs will be converted to FALSE so this won't
    # be activated.

    # TODO: work on settings to separete ind_to_char and grp_routine
    warning(paste("Some indicators contain missing values.",
                  "To allow missing values as FALSE in indicators,",
                  "set na_as_false = TRUE,",
                  "or use !is.na() to explicitly exclude them in conditions."))
  }

  # There should only be 1s or NAs in rs. 0s need to be converted to NA
  ind_df[is.na(rs) | (!is.na(rs) & rs < 1), ] <- NA_integer_

  char_vec <- unname(from)[as.matrix(ind_df) %*% seq_along(from)]

  if (ret_factor) char_vec <- factor(char_vec, levels = from)

  # Check new tidyr see if this still append_col
  first_col <- which(names(data) %in% from)[1]
  ret <- append_col(data, char_vec, col, first_col - 1)

  # Check whether this should be put into a method
  # Give back groups
  if (dplyr::is.grouped_df(data))
    ret <- dplyr::group_by_(ret, .dots = dplyr::groups(data))

  if (remove) ret <- ret[setdiff(names(ret), from)]

  ret
}


#' @export
as_indicator <- function(x, convert_na = FALSE, ...) {
  # Indicator is an integer vector with only 0 and 1 as entries.
  # According to coercion rule, logical - integer - double - character,
  # integer and double, 0 -> FALSE, otherwise TRUE
  # character "TRUE", "FALSE", "T", "F" will be coerced, otherwise NA.
  UseMethod("as_indicator")
}

#' @export
as_indicator.default <- function(x, convert_na = FALSE) {
  # convert_na is a switch to add !is.na(x)
  as.integer((!convert_na | !is.na(x)) & as.logical(x))
}



