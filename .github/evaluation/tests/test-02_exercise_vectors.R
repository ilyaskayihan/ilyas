
context("exercises in 02_exercise_vectors.R")


test_that("Exercise 01: Bin Counting", {
  exerciseTest("2.1")
  expect_vector_function_equal(ex01BinCounting, .expected = c(Youth = 3, "Young Adult" = 4, Adult = 4, Senior = 4),
                               numbers = c(10.3, 32.7, 50.5, 62.4, 32.0, 50.4, 19.7, 60.1, 69.0, 50.6, 11.1, 48.6, 17.4, 34.3, 78.7),
                               cutoffs = c(Youth = 18, "Young Adult" = 36, Adult = 56, Senior = Inf))

  expect_vector_function_equal(ex01BinCounting,
                               .expected = c(Youth = 0, "Young Adult" = 2, Adult = 2, Senior = 2),
                               numbers = c(18, 18, 36, 36, 100, 200),
                               cutoffs = c(Youth = 18, "Young Adult" = 36, Adult = 56, Senior = Inf))

  expect_vector_function_equal(ex01BinCounting,
                               .expected = c("Severely underweight" = 0, Underweight = 0, Healthy = 2, Overweight = 1),
                               numbers = c(20, 23, 28),
                               cutoffs = c("Severely underweight" = 16, Underweight = 18.5, Healthy = 25, Overweight = Inf))

  expect_vector_function_equal(ex01BinCounting,
                               .expected = c("Severely underweight" = 1, Underweight = 0, Healthy = 0, Overweight = 0),
                               numbers = 15,
                               cutoffs = c("Severely underweight" = 16, Underweight = 18.5, Healthy = 25, Overweight = Inf))

  for (probs in c(.5, .1, .01)) {
    counts <- rbinom(10, 20, probs)
    counts[[1]] <- counts[[1]] + 1
    bins <- sort(c(-200, runif(9, -100, 100), 200))
    numbers <- lapply(seq_len(length(bins) - 1), function(position) {
      runif(counts[position], bins[position], bins[position + 1])
    })

    numbers <- unlist(numbers)[sample.int(sum(counts))]
    bins <- bins[seq(2, length(bins))]
    names(bins) <- sprintf("NO_%s", seq_along(bins))
    names(counts) <- sprintf("NO_%s", seq_along(counts))
    expect_vector_function_equal(ex01BinCounting,
                                 .expected = counts,
                                 numbers = numbers,
                                 cutoffs = bins)
  }

})

test_that("Exercise 02: Data Binning", {
  exerciseTest("2.2")
  expect_vector_function_equal(ex02Binning,
                               numbers = c(10.3, 32.7, 50.5, 62.4, 32.0, 50.4, 19.7, 60.1, 69.0, 50.6, 11.1, 48.6, 17.4, 34.3, 78.7),
                               cutoffs = c(Youth = 18, "Young Adult" = 36, Adult = 56, Senior = Inf),
                               .expected = ordered(c("Youth", "Young Adult", "Adult", "Senior", "Young Adult", "Adult",  "Young Adult", "Senior",
                                                   "Senior", "Adult", "Youth", "Adult",  "Youth", "Young Adult", "Senior"),
                                                 levels = c("Youth", "Young Adult", "Adult", "Senior")),
                               class = "factor")

  expect_vector_function_equal(ex02Binning,
                               numbers = c(20, 23, 28),
                               cutoffs = c("Severely underweight" = 16, Underweight = 18.5, Healthy = 25, Overweight = Inf),
                               .expected = ordered(c("Healthy", "Healthy", "Overweight"),
                                                  levels = c("Severely underweight", "Underweight", "Healthy", "Overweight")))

  expect_vector_function_equal(ex02Binning,
                               numbers = 15,
                               cutoffs = c("Severely underweight" = 16, Underweight = 18.5, Healthy = 25, Overweight = Inf),
                               .expected = ordered(c("Severely underweight"),
                                                  levels = c("Severely underweight", "Underweight", "Healthy", "Overweight")))

  for (probs in c(.5, .1, .01)) {
    counts <- rbinom(10, 20, probs)
    counts[[1]] <- counts[[1]] + 1
    bins <- c(-200, runif(9, -100, 100), 200)
    labels <- LETTERS[order(bins)[2:11]]
    bins <- sort(bins)
    numbers <- lapply(seq_len(length(bins) - 1), function(position) {
      runif(counts[position], bins[position], bins[position + 1])
    })

    numbers <- unlist(numbers)
    orders <- sample.int(length(numbers))

    numbers <- numbers[orders]
    result <- ordered(rep(labels, counts)[orders], levels = labels)
    bins <- bins[seq(2, length(bins))]
    names(bins) <- labels
    expect_vector_function_equal(ex02Binning,
                                 numbers = numbers,
                                 cutoffs = bins,
                                 .expected = result)
  }
})

test_that("Exercise 03: Fizz Buzz", {
  exerciseTest("2.3")
  expect_vector_function_equal(ex03FizzBuzz,
                               up.to = 10, fizz.number = 3, buzz.number = 5,
                               .expected = c("1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz"),
                               class = "character")

  expect_vector_function_equal(ex03FizzBuzz,
                               up.to = 6, fizz.number = 2, buzz.number = 4,
                               .expected = c("1", "Fizz", "3", "Fizz Buzz", "5", "Fizz"))

  expect_vector_function_equal(ex03FizzBuzz,
                               up.to = 10, fizz.number = 1, buzz.number = 1,
                               .expected = rep("Fizz Buzz", 10))

  expect_vector_function_equal(ex03FizzBuzz,
                               up.to = 20, fizz.number = 2, buzz.number = 1,
                               .expected = rep(c("Buzz", "Fizz Buzz"), 10))

  res <- seq(1, 100)
  fInd <- which(res %% 10 == 0 & res %% 3 != 0)
  bInd <- which(res %% 10 != 0 & res %% 3 == 0)
  fbInd <- which(res %% 10 == 0 & res %% 3 == 0)
  res[fInd] <- "Fizz"
  res[bInd] <- "Buzz"
  res[fbInd] <- "Fizz Buzz"
  expect_vector_function_equal(ex03FizzBuzz,
                               up.to = 100, fizz.number = 10, buzz.number = 3,
                               .expected = res)

})
