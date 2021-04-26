context("Remove equivalent columns")

data(iris)

iris$Sepal.Length2 <- 3 * iris$Sepal.Length + 3
remove_equiv_columns(iris)
expect_true(equiv(iris[-1], remove_equiv_columns(iris, "Sepal.Length2")))

