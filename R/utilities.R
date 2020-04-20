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
append_df <- function (x, y, after = length(x), remove = FALSE) {
  if (is.character(after)) {
    after <- match(after, dplyr::tbl_vars(x))
  } else if (!is.integer(after)) {
    stop("`after` must be character or integer", call. = FALSE)
  }

  x_vars <- setdiff(names(x), names(y))

  if (remove) {
    x_vars <- setdiff(x_vars, names(x)[[after]])
    after <- after - 1L
  }

  y <- append(x[x_vars], y, after = after)
  structure(y, class = class(x), row.names = .row_names_info(x, 0L))
}

append_col <- function (x, col, name, after = length(x)) {
  name <- enc2utf8(name)
  new_col <- list(col)
  names(new_col) <- name
  append_df(x, new_col, after = after)
}


