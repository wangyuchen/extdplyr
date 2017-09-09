# Truth table used by ind_to_char
truth_table <- expand.grid(P = c(TRUE, FALSE, NA), Q = c(TRUE, FALSE, NA))
truth_table$IND <- c("Error", "Q", NA, "P", NA, NA, NA, NA, NA)
truth_table$IND_na_as_false <-
  c("Error", "Q", "Q", "P", NA, NA, "P", NA, NA)
truth_table

truth_table %>% slice(-1) %>%
  ind_to_char(IND2, P, Q, remove = FALSE) %>%
  ind_to_char(IND2_na, P, Q, na_as_false = TRUE, remove = FALSE)


# Supports converting the following atomic types to indicator
df <- data.frame(integer_ind = c(2L, 0L, 0L, 0L, 0L, 0L, 0L),
                 # non-zero integer is 1, otherwise 0.
                 logcal_ind = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
                 # TRUE is 1.
                 double_ind = c(0, 0, 2.0, 0, 0, 0, 0),
                 # non-zero double is 1.
                 char_ind = c("FALSE", "FALSE", "F", "TRUE", "T", "F", "F"),
                 # "T" and "TRUE" converts to 1.
                 factor_ind = factor(c(1, 1, 1, 1, 1, 0, 1), levels = c(0, 1),
                                     labels = c(TRUE, FALSE)),
                 # Factors are converted on levels, see as.logical.factor()
                 # Note that here 0 = TRUE, 1 = FALSE, but 0 is converted to
                 # logical TRUE.
                 stringsAsFactors = FALSE)

ind_to_char_(df, col = "new_y", from = names(df), remove = FALSE)


# ind_to_char as complement to use model.matrix on a factor
df <- data.frame(x = 1:6, y = factor(c(letters[1:5], NA)))
ind_df <- as.data.frame(model.matrix(~ x + y - 1,
                                      model.frame(df, na.action = na.pass)))
ind_df  # an indicator matrix with NAs

# New character column is generated with non-selected columns kept as is.
ind_to_char(ind_df, new_y, ya:ye, remove = FALSE)
ind_to_char(ind_df, new_y, ya:ye)
ind_to_char(ind_df, new_y, -x)
# Returns a factor column
ind_to_char(ind_df, col = new_y, ya:ye, ret_factor = TRUE)

# Using SE
ind_to_char_(ind_df, col = "new_y", from = c("ya", "yb", "yc", "yd", "ye"))
