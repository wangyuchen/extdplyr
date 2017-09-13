wide_table <- iris
wide_table$id <- 1:nrow(iris)
tidy_routine(wide_table, into = c("part", "var"),
             regex = "(Sepal|Petal)\\.(Length|Width)")
