usethis::use_testthat()

# In tests/testthat/test-influence_plot.R
test_that("influence_plot works with valid inputs", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  plot <- influence_plot(mtcars, model)
  expect_s3_class(plot, "ggplot")
})

test_that("influence_plot throws error with invalid model", {
  data(mtcars)
  expect_error(influence_plot(mtcars, mtcars), "model must be an object of class lm")
})
