#' Calculate percentage by group.
#'
#' \code{pct_routine} works like \code{\link[dplyr]{count}} except that it
#' returns group percentages instead of counts. \code{tally_pct} is a underlying
#' utility function that corresponds to \code{tally}. As the name implies, it
#' also returns percentage.
#'
#' @inheritParams common_params
#' @param ... Variables to group by, see \code{\link[dplyr]{group_by}}.
#' @param wt Bare or quoted column name of weights, see examples for details.
#' @param ret_name Character of the variable name returned.
#' @param margin Margin to calculate percentage for. This is like the margin in
#' \code{\link[base]{prop.table}}.
#' @param rebase Whether to remove the missing values in the percentage, e.g.
#' rebase the percentage so that NAs in the last group are excluded.
#' @param ungroup Whether to ungroup the returned table.
#'
#' @example examples/pct_routine_ex.R
#'
#' @export
pct_routine <- function(data, ..., wt = NULL, ret_name = "pct", margin = 1,
                        rebase = FALSE, ungroup = FALSE) {
  vars <- lazyeval::lazy_dots(...)
  wt <- col_name(substitute(wt))

  pct_routine_(data, vars = vars, wt = wt, ret_name = ret_name,
               margin = margin, rebase = rebase, ungroup = ungroup)
}

#' @param vars A character vector of variable names to group by.
#' @describeIn pct_routine SE version of \code{pct_routine}.
#' @export
pct_routine_ <- function(data, vars, wt = NULL, ret_name = "pct", margin = 1,
                         rebase = FALSE, ungroup = FALSE) {
  grouped <- dplyr::group_by_(data, .dots = vars, add = TRUE)
  ret <- tally_pct_(grouped, wt = wt, ret_name = ret_name,
                    margin = margin, rebase = rebase)
  if (ungroup) dplyr::ungroup(ret) else ret
}


#' @describeIn pct_routine NSE version of \code{tally_pct_}.
#' @export
tally_pct <- function(data, wt = NULL, ret_name = "pct",
                      margin = 1, rebase = FALSE) {
  wt <- col_name(substitute(wt))
  tally_pct_(data, wt = wt, ret_name = ret_name, margin = margin,
             rebase = rebase)
}


#' @describeIn pct_routine Underlying SE function of \code{pct_routine_} without
#' options for groups.
#' @export
tally_pct_ <- function(data, wt = NULL, ret_name = "pct",
                       margin = 1, rebase = FALSE) {

  margin <- as.integer(margin)
  if (margin < 1) stop("Margin must be a positive integer.")

  # wt is quoted name here
  if (is.null(wt)) {
    expr <- quote(n())
  } else {
    expr <- interp(quote(sum(wt, na.rm = TRUE)), wt = as.name(wt))
  }

  last_group <- last_group(data)
  if (rebase & !is.null(last_group)) {
    rebase_expr <- interp(quote(!is.na(last_col)),
                          last_col = last_group)
  } else {
    rebase_expr <- quote(TRUE)
  }


  # It's actually calculating counts (n) instead of percentage in the summarize
  # call, but to minimize the use of internal names to avoid possible name
  # conflicts, it's named as "ret_name" whereas it's still a count.
  if (ret_name %in% dplyr::groups(data))
    stop(paste("Naming conflict of ret_name", ret_name,
               "and a grouping variable in data,",
               "change ret_name to something else."))

  data_groups <- dplyr::groups(data)
  dplyr::summarise_(data, .dots = named_expr(ret_name, expr)) %>%
    dplyr::filter_(rebase_expr) %>%
    dplyr::group_by_(.dots = head(data_groups, -margin)) %>%
    dplyr::mutate_(.dots = named_expr(ret_name,
                                      interp(quote(var / sum(var)),
                                             var = as.name(ret_name))))
}


