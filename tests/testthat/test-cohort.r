context("Cohort testing")

library(dplyr)

data(iris)

iris$Species <- as.character(iris$Species)
iris$s3 <- sample(letters, nrow(iris), replace = TRUE)
iris2 <- iris %>%
  as_tibble %>%
  cohort(on="Species", name = "data")

expect_equal(c(3, 2), dim(iris2))
expect_equal(c(50, 5), dim(iris2$data[[1]]))

iris2 <- iris %>% as_tibble() %>%
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

iris2$rownum <- seq_len(nrow(iris2))
expect_equal(iris2, cohort(iris2, on = "rownum", name = "data"))
