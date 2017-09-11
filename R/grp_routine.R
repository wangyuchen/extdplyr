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
#' @param na_as_false Treat NAs as FALSE in indicators columns.
#'
#' @example examples/ind_to_char_ex.R
#'
#' @export
ind_to_char <- function(data, col, ..., na_as_false = FALSE,
                        ret_factor = FALSE, remove = TRUE) {
  UseMethod("ind_to_char")
}

#' @describeIn ind_to_char Method for data.frame.
#' @export
ind_to_char.data.frame <- function(data, col, ..., na_as_false = FALSE,
                                   ret_factor = FALSE, remove = TRUE) {
  col_name <- rlang::quo_name(enquo(col))
  vars <- quos(...)
  vars <- lapply(vars, rlang::env_bury, UQS(helpers))

  from_vars <- tidyselect::vars_select(colnames(data), UQS(vars))

  ind_df <- data[from_vars]
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


  char_vec <- unname(from_vars)[as.matrix(ind_df) %*% seq_along(from_vars)]

  if (ret_factor) char_vec <- factor(char_vec, levels = from_vars)

  first_col <- which(names(data) %in% from_vars)[1]

  if (remove) names_out <- setdiff(names(data), from_vars)
  else names_out <- names(data)

  ordered_name <- append(names_out, col_name, after = first_col - 1)

  # This setting only works for data.frame method
  # other classes need their own method to add additional information
  data %>%
    dplyr::mutate(UQ(col_name) := char_vec) %>%
    dplyr::select(ordered_name)
}

#' @describeIn ind_to_char Method for grouped_df.
#' @export
ind_to_char.grouped_df <- function(data, col, ..., na_as_false = FALSE,
                                   ret_factor = FALSE, remove = TRUE) {
  col_name <- rlang::quo_name(enquo(col))
  from_vars <- tidyselect::vars_select(colnames(data), ...)

  ret <- ind_to_char(as.data.frame(data), UQ(col_name), UQS(from_vars),
                     na_as_false = na_as_false, ret_factor = ret_factor,
                     remove = remove)

  dplyr::group_by(ret, UQS(dplyr::groups(data)), add = TRUE)
}

#' @export
ind_to_char_ <- function(data, col, from, na_as_false = FALSE,
                         ret_factor = FALSE, remove = TRUE) {
  UseMethod("ind_to_char_")
}


#' @export
ind_to_char_.data.frame <- function(data, col, from, na_as_false = FALSE,
                                    ret_factor = FALSE, remove = TRUE) {
  col_name <- rlang::quo_name(enquo(col))
  from <- rlang::syms(from)

  ind_to_char(data, UQ(col), UQS(from),
              na_as_false = na_as_false, ret_factor = ret_factor,
              remove = remove)
}


#' Convert Vectors to Indicators
#'
#' Indicators are binary integers where 0 stands for FALSE and 1 stands for
#' TRUE. This generic is called by \code{ind_to_char} to convert variables into
#' indicators. User can extend \code{ind_to_char} to support user-defined types
#' by providing methods to this generic.
#'
#' @param x Vector to be converted
#' @param convert_na Whether NAs should be converted to FALSE.
#' @param ... Specific arguments passed to methods.
#' @export
as_indicator <- function(x, convert_na = FALSE, ...) {
  # Indicator is an integer vector with only 0 and 1 as entries.
  # According to coercion rule, logical - integer - double - character,
  # integer and double, 0 -> FALSE, otherwise TRUE
  # character "TRUE", "FALSE", "T", "F" will be coerced, otherwise NA.
  UseMethod("as_indicator")
}

#' @describeIn as_indicator Default method that works on atomic types.
#' @export
as_indicator.default <- function(x, convert_na = FALSE, ...) {
  # convert_na is a switch to add !is.na(x)
  as.integer((!convert_na | !is.na(x)) & as.logical(x))
}



