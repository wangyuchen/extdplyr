df <- data.frame(v1 = letters[1:5], v2 = 1:5)
df

# By default, it creates new groups
grp_routine(df, group,
            first = v1 %in% c("a", "b"),
            second = v2 == 3,
            third = v2 >= 4)

# Un-named conditions are auto named
grp_routine(df, group,
            v1 %in% c("a", "b"),
            v2 == 3,
            v2 >= 4)


\dontrun{
# stops when conditions overlap so groups are not mutually exclusive
grp_routine(df, group,
            first = v1 %in% c("a", "b"),
            second = v2 == 3,
            third = v2 >= 3)
}

# arguments work with standard evaluation
grp_routine(df, "group",
            "first" = v1 %in% c("a", "b"),
            "second" = v2 == 3,
            "third" = v2 >= 4)

# Missing values in conditions are treated with na_as_false = TRUE
df$v2 <- c(1:4, NA)
df

grp_routine(df, group,
            first = v1 %in% c("a", "b"),
            second = v2 == 3,
            third = v1 == "e" | v2 >= 4)

grp_routine(df, group,
            first = v1 %in% c("a", "b"),
            second = v2 == 3,
            third = v1 == "e" | v2 >= 4,
            na_as_false = TRUE)
