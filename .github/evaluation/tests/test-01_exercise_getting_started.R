
context("exercises in 01_exercise_getting_started.R")

test_that("Exercise 01: Divide", {
  exerciseTest("1.1")
  expect_vector_function_equal(ex01Divide, .expected = 1, x = 2, y = 2)
  expect_vector_function_equal(ex01Divide, .expected = 2, x = 10, y = 5)
  expect_vector_function_equal(ex01Divide, .expected = -2, x = 10, y = -5)
  expect_vector_function_equal(ex01Divide, .expected = -2, x = -10, y = 5)
  dividends <- rnorm(5)
  divisors <- rnorm(5)
  res <- dividends / divisors
  for (i in seq(1, 5)) {
    expect_vector_function_equal(ex01Divide, .expected = res[i], x = dividends[i],
                                 y = divisors[i])
  }
})

test_that("Exercise 02: Divide Vectors", {
  exerciseTest("1.2")
  expect_vector_function_equal(ex02DivideVectors, .expected = c(1, 2), x = c(2, 4),
                               y = c(2, 2), class = "numeric")
  expect_vector_function_equal(ex02DivideVectors, .expected = c(2, -4), x = c(10, 20),
                               y = c(5, -5))
  expect_vector_function_equal(ex02DivideVectors, .expected = -2, x = 10,
                               y = -5)
  expect_vector_function_equal(ex02DivideVectors, .expected = c(-2, -2), x = c(-10, -10),
                               y = 5)
  dividends <- rnorm(10)
  divisors <- rnorm(10)
  expect_vector_function_equal(ex02DivideVectors, .expected = dividends / divisors,
                               x = dividends, y = divisors)
})


test_that("Exercise 03: Vector Type", {
  exerciseTest("1.3")
  expect_vector_function_equal(ex03VectorType, .expected = "L", TRUE,
                               class = "character")
  expect_vector_function_equal(ex03VectorType, .expected = "L", FALSE)
  expect_vector_function_equal(ex03VectorType, .expected = "N", 1.0)
  expect_vector_function_equal(ex03VectorType, .expected = "N", 0.0)
  expect_vector_function_equal(ex03VectorType, .expected = "N", -0.1)
  expect_vector_function_equal(ex03VectorType, .expected = "C", "x")
  expect_vector_function_equal(ex03VectorType, .expected = "C", "")
  expect_vector_function_equal(ex03VectorType, .expected = "L", 
                               sample(c(TRUE, FALSE), 10, TRUE))
  expect_vector_function_equal(ex03VectorType, .expected = "L", logical(0),
                               add.err.info = "Make sure your function works with empty vectors aswell.")
  expect_vector_function_equal(ex03VectorType, .expected = "N", runif(10))
  expect_vector_function_equal(ex03VectorType, .expected = "N", numeric(0),
                               add.err.info = "Make sure your function works with empty vectors aswell.")
  expect_vector_function_equal(ex03VectorType, .expected = "C", letters)
  expect_vector_function_equal(ex03VectorType, .expected = "C", character(0),
                               add.err.info = "Make sure your function works with empty vectors aswell.")
  expect_vector_function_equal(ex03VectorType, .expected = "X", NULL)
  expect_vector_function_equal(ex03VectorType, .expected = "X", 1+2i)
  expect_vector_function_equal(ex03VectorType, .expected = "X", .GlobalEnv)
  # no longer results in X this way but in N, just taken out for now
  # expect_vector_function_equal(ex03VectorType, .expected = "X", quote(1 + 1))
  expect_vector_function_equal(ex03VectorType, .expected = "X", list(1, 2, 3))
  expect_vector_function_equal(ex03VectorType, .expected = "X", list())
})


test_that("Exercise 04: Even Numbers", {
  exerciseTest("1.4")
  expect_vector_function_equal(ex04Even, .expected = (1:5) * 2, 1:10)
  expect_vector_function_equal(ex04Even, .expected = numeric(0), (-10:3) * 2 + 1)
  expect_vector_function_equal(ex04Even, .expected = (1:5) * 2, 1:10)
  expect_vector_function_equal(ex04Even, .expected = 2, 2)
  expect_vector_function_equal(ex04Even, .expected = numeric(0), 1)
})