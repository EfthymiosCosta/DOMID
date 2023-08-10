ex_case1 <- elbow_angle(vec = sort(rnorm(10), decreasing = TRUE), range = c(1:10))

test_that("Correct output object.", {
  expect_equal(is.numeric(ex_case1), TRUE)
  expect_equal(length(ex_case1), 1)
})
test_that("Incorrect input argument values.", {
  expect_error(elbow_angle(vec = sort(rnorm(9), decreasing = TRUE), range = c(1:10)),
               "The 2 parameters must be vectors of the same length.", fixed = TRUE)
  expect_error(elbow_angle(vec = sort(rnorm(10), decreasing = TRUE), range = c(1:19)),
               "The 2 parameters must be vectors of the same length.", fixed = TRUE)
  expect_error(elbow_angle(vec = sort(rnorm(3), decreasing = TRUE), range = c(1:3)),
               "Input arguments should be vectors of length at least equal to 4.", fixed = TRUE)
  expect_error(elbow_angle(vec = sort(rnorm(10), decreasing = TRUE), range = rep('hi', 10)),
               "Input arguments should be vectors of class 'numeric'.", fixed = TRUE)
  expect_error(elbow_angle(vec = rep('hi', 10), range = c(1:10)),
               "Input arguments should be vectors of class 'numeric'.", fixed = TRUE)
  expect_error(elbow_angle(vec = sort(rnorm(10), decreasing = FALSE), range = c(1:10)),
               "Vector of values should consist of decreasing values.", fixed = TRUE)
  expect_error(elbow_angle(vec = rnorm(10), range = c(1:10)),
               "Vector of values should consist of decreasing values.", fixed = TRUE)
  expect_error(elbow_angle(vec = sort(rnorm(10), decreasing = TRUE), range = c(1:9, 9)),
               "Range vector should consist of strictly increasing unique values.", fixed = TRUE)
  expect_error(elbow_angle(vec = sort(rnorm(10), decreasing = TRUE), range = c(1:9, 0)),
               "Range vector should consist of strictly increasing unique values.", fixed = TRUE)
  expect_error(elbow_angle(vec = sort(rnorm(10), decreasing = TRUE), range = c(1:9, 8)),
               "Range vector should consist of strictly increasing unique values.", fixed = TRUE)
  expect_error(elbow_angle(vec = sort(rnorm(10), decreasing = TRUE), range = c(1:9, -9)),
               "Range vector should consist of strictly increasing unique values.", fixed = TRUE)
})
