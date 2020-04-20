library(extdplyr)
context("ind_to_char_ and grp_routine_")

test_that("ind_to_char_ works with regular data.frame", {
  df <- data.frame(x = 1:5, y = factor(c(letters[1:5])))
  ind_df <- as.data.frame(model.matrix(~ x + y - 1, df))

  # Using SE
  df_ret <- ind_to_char_(ind_df, col = "new_y",
                         from = c("ya", "yb", "yc", "yd", "ye"))

  expect_equal(ncol(df_ret), 2)
  expect_equal(df_ret[['new_y']], c("ya", "yb", "yc", "yd", "ye"))

  df_ret2 <- ind_to_char_(ind_df, col = "new_y",
                          from = c("ya", "yb", "yc", "yd", "ye"),
                          remove = FALSE)
  expect_equal(ncol(df_ret2), 7)
  expect_equal(df_ret2[["new_y"]], c("ya", "yb", "yc", "yd", "ye"))
})


test_that("ind_to_char_ works with non-integer indicators", {
  df <- data.frame(integer_ind = c(1L, 0L, 0L, 0L, 0L, 0L),
                   logcal_ind = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
                   double_ind = c(0, 0, 2.0, 0, 0, 0),
                   char_ind = c("FALSE", "FALSE", "F", "TRUE", "T", "FALSE"),
                   factor_ind = factor(c(1, 1, 1, 1, 1, 0), levels = c(0, 1),
                                       labels = c(TRUE, FALSE)),
                   stringsAsFactors = FALSE)

  # Using SE
  df_ret <- ind_to_char_(df, col = "new_y", from = names(df), remove = FALSE)

  expect_equal(ncol(df_ret), 6)
  expect_equal(which(names(df_ret) == "new_y"), 1)
  expect_equal(df_ret[['new_y']],
               c("integer_ind", "logcal_ind", "double_ind", "char_ind",
                 "char_ind", "factor_ind"))

})



library(dplyr)
test_that("ind_to_char_ works with tbl_df, tbl, data.frame", {
  df <- data.frame(x = 1:5, y = factor(c(letters[1:5])))
  ind_df <- as_tibble(model.matrix(~ x + y - 1, df))

  # Using SE
  df_ret <- ind_to_char_(ind_df, col = "new_y",
                         from = c("ya", "yb", "yc", "yd", "ye"))

  expect_equal(class(ind_df), class(df_ret))
  expect_equal(ncol(df_ret), 2)
  expect_equal(df_ret[['new_y']], c("ya", "yb", "yc", "yd", "ye"))

  df_ret2 <- ind_to_char_(ind_df, col = "new_y",
                          from = c("ya", "yb", "yc", "yd", "ye"),
                          remove = FALSE)

  expect_equal(class(ind_df), class(df_ret2))
  expect_equal(ncol(df_ret2), 7)
  expect_equal(df_ret2[["new_y"]], c("ya", "yb", "yc", "yd", "ye"))
})


test_that("ind_to_char_ works with grouped_df, tbl_df, tbl, data.frame", {
  df <- data.frame(x = 1:5, y = factor(c(letters[1:5])))
  ind_df <- as_tibble(model.matrix(~ x + y - 1, df)) %>%
    group_by(z = x > 3)

  # Using SE
  df_ret <- ind_to_char_(ind_df, col = "new_y",
                         from = c("ya", "yb", "yc", "yd", "ye"))

  expect_equal(class(ind_df), class(df_ret))
  expect_equal(ncol(df_ret), 3)
  expect_equal(df_ret[['new_y']], c("ya", "yb", "yc", "yd", "ye"))

  df_ret2 <- ind_to_char_(ind_df, col = "new_y",
                          from = c("ya", "yb", "yc", "yd", "ye"),
                          remove = FALSE)

  expect_equal(class(ind_df), class(df_ret2))
  expect_equal(ncol(df_ret2), 8)
  expect_equal(df_ret2[["new_y"]], c("ya", "yb", "yc", "yd", "ye"))
})


if (requireNamespace("data.table", quietly = TRUE)) {
  test_that("ind_to_char_ works with tbl_df, tbl, data.frame", {
    df <- data.frame(x = 1:5, y = factor(c(letters[1:5])))
    ind_df <- data.table::data.table(model.matrix(~ x + y - 1, df))

    # Using SE
    df_ret <- ind_to_char_(ind_df, col = "new_y",
                           from = c("ya", "yb", "yc", "yd", "ye"))

    expect_equal(class(ind_df), class(df_ret))
    expect_equal(ncol(df_ret), 2)
    expect_equal(df_ret[['new_y']], c("ya", "yb", "yc", "yd", "ye"))

    df_ret2 <- ind_to_char_(ind_df, col = "new_y",
                            from = c("ya", "yb", "yc", "yd", "ye"),
                            remove = FALSE)

    expect_equal(class(ind_df), class(df_ret2))
    expect_equal(ncol(df_ret2), 7)
    expect_equal(df_ret2[["new_y"]], c("ya", "yb", "yc", "yd", "ye"))
  })
}



