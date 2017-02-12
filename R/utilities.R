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
#' @keywords internal
common_params <- function(data, col) {
  NULL
}
