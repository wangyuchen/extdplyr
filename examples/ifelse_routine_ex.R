df <- data.frame(v1 = letters[1:5], v2 = 1:5)
df

# By default, it creates new groups
ifelse_routine(df, "group",
               first = v1 %in% c("a", "b"),
               second = v2 == 3,
               third = v2 >= 4)

# Gives a warning when the groups are not collectively exhaustive
ifelse_routine(df, "group",
               first = v1 %in% c("a", "b"),
               second = v2 == 3,
               third = v2 > 4)


# SE version
ifelse_routine_(df, "group",
               "first" = ~ v1 %in% c("a", "b"),
               "second" = ~ v2 == 3,
               .dots = setNames(list(~ v2 > 4), "third"))
