# Truth table used by ind_to_char
truth_table <- expand.grid(P = c(TRUE, FALSE, NA), Q = c(TRUE, FALSE, NA))
truth_table$IND <- c("Error", "Q", NA, "P", NA, NA, NA, NA, NA)
truth_table$IND_na_as_false <-
  c("Error", "Q", "Q", "P", NA, NA, "P", NA, NA)
truth_table


# Supports converting the following atomic types to indicator
df <- data.frame(integer_ind = c(2L, 0L, 0L, 0L, 0L, 0L, 0L),
                 # non-zero integer is 1, otherwise 0.
                 logical_ind = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
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

# Keep original columns with remove = FALSE
ind_to_char(df, col = new_y, integer_ind, logical_ind, double_ind,
            char_ind, factor_ind, remove = FALSE)



# ind_to_char as complement to use model.matrix on a factor
model_df <- data.frame(x = 1:6, y = factor(c(letters[1:5], NA)))
model_df
ind_df <- as.data.frame(model.matrix(~ x + y - 1,
                                     model.frame(model_df,
                                                 na.action = na.pass)))
ind_df  # an indicator matrix with NAs

# Convert back to one column like model_df
ind_to_char(ind_df, new_y, ya, yb, yc, yd, ye)

# Returns a factor column
ind_to_char(ind_df, col = new_y, ya:ye, ret_factor = TRUE)


# Use select helper functions as in dplyr::select
ind_to_char(ind_df, new_y, contains("y"), remove = FALSE)
ind_to_char(ind_df, new_y, starts_with("y"), remove = FALSE)

# Using SE
ind_to_char(ind_df, col = "new_y", "ya", "yb", "yc", "yd", "ye")
