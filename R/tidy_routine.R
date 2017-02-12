tidy_routine <- function(df, into, regex, spread = "var") {
  # col must have an element called var
  stopifnot(spread %in% into)
  suppressWarnings({
    # suppress attributes warning
    df %>%
      tidyr::gather(var_name, val, matches(regex)) %>%
      tidyr::extract(var_name, into = into, regex = regex, convert = TRUE) %>%
      tidyr::spread_(spread, "val")

  })
}
