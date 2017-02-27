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
grp_routine <- function(data, col, ..., ret_factor = FALSE) {
  grp_routine_(data, col, .dots = lazyeval::lazy_dots(...),
                  ret_factor = ret_factor)
}

#' @describeIn grp_routine SE version of grp_routine.
#' @export
grp_routine_ <- function(data, col, ..., .dots, ret_factor = FALSE) {
  conds <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  data %>%
    dplyr::mutate_(.dots = conds) %>%
    ind_to_char_(col, names(conds), ret_factor = ret_factor,
                 remove = TRUE, mutually_exclusive = TRUE,
                 collectively_exhaustive = TRUE)
}




#' Convert indicator data.frame to character/factor.
#'
#' This is the reverse operation of using \code{\link[stats]{model.matrix}} a
#' factor. \code{ind_to_char} works like \code{dplyr::unite}, it combines
#' multiple indicator columns into one character/factor column and add it to
#' the data.
#'
#' @inheritParams common_params
#' @inheritParams tidyr::unite_
#' @param ... Specification of indicator columns. Use bare variable names.
#' Select all variables between \code{x} and \code{z} with \code{x:z}. For more
#' options, see the \code{\link[dplyr]{select}} documentation.
#' @param ret_factor Whether to convert the column into factor.
#' @param mutually_exclusive Check if the indicators are mutually exclusive.
#' @param collectively_exhaustive Check if the indicators are collectively
#' exhaustive.
#'
#' @example examples/ind_to_char_ex.R
#'
#' @export
ind_to_char <- function(data, col, ..., ret_factor = FALSE, remove = TRUE,
                        mutually_exclusive = TRUE,
                        collectively_exhaustive = TRUE) {
  col <- deparse(substitute(col))
  from <- dplyr::select_vars(colnames(data), ...)
  ind_to_char_(data, col, from, ret_factor = ret_factor, remove = remove,
               mutually_exclusive = mutually_exclusive,
               collectively_exhaustive = collectively_exhaustive)
}

#' @describeIn ind_to_char SE version of \code{ind_to_char}.
#' @export
ind_to_char_ <- function(data, col, from, ret_factor = FALSE, remove = TRUE,
                         mutually_exclusive = TRUE,
                         collectively_exhaustive = TRUE) {
  # check if it's indicator. Indicators should be integer 0 or 1.
  # According to coercion rule, logical - integer - double - character,
  # Here convert to logical first for safety.

  int_df <- data[from]
  int_df[] <- lapply(int_df, function(x) as.integer(as.logical(x)))

  rs <- rowSums(int_df)

  if (mutually_exclusive) {
   if (any(rs > 1, na.rm = TRUE)) {
     # Have to stop here, because don't know which one to take
     stop("Indicators are not mutually exclusive, check overlaps.")
   }
  }

  if (collectively_exhaustive) {
    if (any(rs < 1, na.rm = TRUE)) {
      int_df[rs < 1, ] <- NA_integer_
      warning("Indicators are not collectively exhaustive, NAs introduced.")
    }
  }


  char_vec <- from[as.matrix(int_df) %*% seq_along(from)]

  if (ret_factor) char_vec <- as.factor(char_vec)

  first_col <- which(names(data) %in% from)[1]
  ret <- append_col(data, char_vec, col, first_col - 1)

  # Give back groups
  if (dplyr::is.grouped_df(data))
    ret <- dplyr::group_by_(ret, .dots = dplyr::groups(data))

  if (remove) ret <- ret[setdiff(names(ret), from)]

  ret
}




