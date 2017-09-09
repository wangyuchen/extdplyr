df <- data.frame(v1 = letters[1:5], v2 = 1:5)
df

# By default, it creates new groups
grp_routine(df, group,
            first = v1 %in% c("a", "b"),
            second = v2 == 3,
            third = v2 >= 4)

# Gives a warning when the groups are not collectively exhaustive
grp_routine(df, group,
            first = v1 %in% c("a", "b"),
            second = v2 == 3,
            third = v2 > 4)

\dontrun{
# stops when conditions overlap so groups are not mutually exclusive
grp_routine(df, group,
            first = v1 %in% c("a", "b"),
            second = v2 == 3,
            third = v2 >= 3)
}

# SE version
grp_routine_(df, "group",
             "first" = ~ v1 %in% c("a", "b"),
             "second" = ~ v2 == 3,
             .dots = setNames(list(~ v2 > 4), "third"))

# Missing values in conditions are treated with missing_as_false = TRUE
df <- data.frame(v1 = c(letters[1:4], NA))
df

grp_routine(df, group,
            first = v1 == "a",
            second = v1 != "a",
            na_as_false = TRUE)
