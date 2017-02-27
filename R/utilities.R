named_expr <- function(name, expr) {
  stats::setNames(list(expr), nm = name)
}

last_group <- function(x) {
  groups <- dplyr::groups(x)
  groups[[length(groups)]]
}

check_se_column <- function(col_name) {
  stopifnot(as.character(col_name), length(col) == 1)
}



#' Common parameters across routines
#'
#' @param data A \code{\link[base]{data.frame}} or \code{\link[dplyr]{tbl}}.
#' @param col Name of the generated column. Use a bare name when using NSE
#' functions and a character (quoted) name when using SE functions (functions
#' that end with underscores).
#' @param .dots Used in conjunction with \code{...} to support both explicit
#' and implicit arguments.
#' @keywords internal
common_params <- function(data, col, .dots) {
  NULL
}


# Adapted from tidyr
append_df <- function (x, values, after = length(x)) {
  y <- append(x, values, after = after)
  class(y) <- class(x)
  attr(y, "row.names") <- attr(x, "row.names")
  y
}

append_col <- function (x, col, name, after = length(x)) {
  name <- enc2utf8(name)
  append_df(x, named_expr(name, col), after = after)
}


