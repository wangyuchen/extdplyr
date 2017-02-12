# ind_to_char as complement to use model.matrix on a factor
df <- data.frame(x = 1:6, y = factor(c(letters[1:5], NA)))
ind_df <- as.data.frame(model.matrix(~ x + y - 1,
                                      model.frame(df, na.action = na.pass)))
ind_df  # an indicator matrix with NAs

# New character column is generated with non-selected columns kept as is.
ind_to_char(ind_df, new_y, ya:ye)
ind_to_char(ind_df, new_y, -x)
ind_to_char(ind_df, col = new_y, ya:ye, remove = FALSE)
# Returns a factor column
ind_to_char(ind_df, col = new_y, ya:ye, ret_factor = TRUE)

# Using SE
ind_to_char_(ind_df, col = "new_y", from = c("ya", "yb", "yc", "yd", "ye"))
