
context("exercises in 03_exercise_matrices_and_dataframes.R")

test_that("Exercise 01: Counterdiagonal", {
  exerciseTest("3.1")
  expect_matrix_function_equal(ex01Counterdiagonal, diag(3), .expected = c(0, 1, 0))
  expect_matrix_function_equal(ex01Counterdiagonal, matrix(1:9, ncol = 3, byrow = TRUE),
                      .expected = c(3, 5, 7))
  x <- rnorm(100)
  expect_matrix_function_equal(ex01Counterdiagonal, matrix(x, ncol = 10),
                      .expected = rev(x[1 + (1:10 * 9)]))
  expect_matrix_function_equal(ex01Counterdiagonal, matrix(1), .expected = 1)
  expect_matrix_function_equal(ex01Counterdiagonal, matrix(10), .expected = 10)
})

test_that("Exercise 02: Matrix-which.max", {
  exerciseTest("3.2")
  expect_matrix_function_equal(ex02MatrixWhichMax, matrix(1:9, ncol = 3, byrow = TRUE),
                      .expected = c(3, 3))
  expect_matrix_function_equal(ex02MatrixWhichMax, matrix(c(0, 0, 3, 0, 0, 0), nrow = 3),
                      .expected = c(3, 1))
  tryCatch(expect_matrix_function_equal(ex02MatrixWhichMax, diag(2),
                      .expected  = c(2, 2)),
          error = function (e) {
              expect_matrix_function_equal(ex02MatrixWhichMax, diag(2),
                                 .expected = c(1, 1))
           })

  x <- rnorm(100)
  xmat <- matrix(x, ncol = 10)
  expect_matrix_function_equal(ex02MatrixWhichMax, xmat,
                      .expected = as.numeric(which(xmat == max(xmat),
                                                  arr.ind = TRUE)))
})

test_that("Exercise 03: Bag of Words", {
  exerciseTest("3.3")
  .expected <- matrix(c(1, 0, 0, 2, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1), nrow = 3)
  colnames(.expected) <- c("THE", "HOUSE", "WAS", "A", "SMALL", "GERHARD", "IN", "MAN")

  expect_matrix_function_equal(ex03BagOfWords, c("THE HOUSE WAS A SMALL HOUSE", "GERHARD WAS IN A HOUSE", "GERHARD WAS A SMALL MAN")
                      , .expected = .expected, .ignore.col.order = TRUE,
                      .ignore.row.order = TRUE,
                      add.err.info = "Make sure to correctly extrapolate the words as colnames!")
  expect_matrix_function_equal(ex03BagOfWords, c("THE HOUSE WAS A SMALL HOUSE", "GERHARD WAS IN A HOUSE", "GERHARD WAS A SMALL MAN")
                               , .expected = .expected, .ignore.col.order = TRUE)

  amat <- matrix(1)
  colnames(amat) <- "A"
  expect_matrix_function_equal(ex03BagOfWords, "A", .expected = amat, class = "matrix")
  expect_matrix_function_equal(ex03BagOfWords, c("A", "A"), .expected = rbind(amat, amat))
  abmat <- diag(2)
  colnames(abmat) <- c("A", "B")
  expect_matrix_function_equal(ex03BagOfWords, c("A", "B"), .expected = abmat,
                               .ignore.col.order = TRUE)

  protosentences <- replicate(30, replicate(20, paste(sample(LETTERS[1:3]), collapse = "")), simplify = FALSE)

  sentences <- sapply(protosentences, paste, collapse = " ")
  allnames <- sort(unique(unlist(protosentences)))
  tables <- lapply(protosentences, function(x) {
    ret <- table(x)[allnames]
    names(ret) <- allnames
    ret[is.na(ret)] <- 0
    ret
  })


  expect_matrix_function_equal(ex03BagOfWords, sentences,
                               .expected = do.call(rbind, lapply(tables, c)),
                               .ignore.col.order = TRUE, class = "matrix")

  for (i in seq_len(min(length(sentences), 5))) {
    expect_matrix_function_equal(ex03BagOfWords, sentences[i],
                                 .expected = as.matrix(t(tables[[i]][tables[[i]] != 0])),
                                 .ignore.col.order = TRUE)
  }

})

test_that("Exercise 04: Select DF", {
  exerciseTest("3.4")
  expect_matrix_function_equal(ex04SelectDF, data = iris, type = "numeric",
                               .expected = iris[1:4], class = "data.frame")
  expect_matrix_function_equal(ex04SelectDF, data = iris, type = "factor",
                               .expected = iris[5])
  expect_matrix_function_equal(ex04SelectDF, data = iris, type = "logical",
                               .expected = iris[FALSE])
  expect_matrix_function_equal(ex04SelectDF, data = iris, type =  c("factor", "numeric"),
                               .expected = iris)
  expect_matrix_function_equal(ex04SelectDF, data = iris, type =  c("numeric", "factor"),
                               .expected = iris)
  expect_matrix_function_equal(ex04SelectDF, data = iris, type =  c("factor", "numeric", "logical"),
                               .expected = iris)
  expect_matrix_function_equal(ex04SelectDF, data = iris, type =  c("factor", "logical", "numeric"),
                               .expected = iris)
  expect_matrix_function_equal(ex04SelectDF, data = cars, type = "numeric",
                               .expected = cars)
  expect_matrix_function_equal(ex04SelectDF, data = cars, type = "factor",
                               .expected = cars[FALSE])

  dfx <- data.frame(x = c(TRUE, TRUE), y = factor(c("x", "y")), z = c(1, 2))
  expect_matrix_function_equal(ex04SelectDF, data = dfx, type = "numeric",
                               .expected = dfx[3])
  expect_matrix_function_equal(ex04SelectDF, data = dfx, type = c("numeric", "logical"),
                               .expected = dfx[c(1, 3)])
  expect_matrix_function_equal(ex04SelectDF, data = dfx, type = c("factor", "logical"),
                               .expected = dfx[c(1, 2)])
})

test_that("Exercise 05: Imputation", {
  exerciseTest("3.5")
    expect_matrix_function_equal(ex05Imputation, data.frame(
    sex = factor(c("male", "male", "male", "female", "female", "female")),
    height = c(178, 185, NA, 157, NA, 174), weight = c(95, 90, 99, 70, 77, NA),
    age = c(23, NA, NA, NA, 21, 22)),
    .expected = data.frame(
      sex = factor(c("male", "male", "male", "female", "female", "female")),
      height = c(178, 185, 181.5, 157, 165.5, 174), weight = c(95, 90, 99, 70, 77, 73.5),
      age = c(23, 23, 23, 21.5, 21, 22)))

    expect_matrix_function_equal(ex05Imputation, data.frame(
      sex = factor(c("male", "female", "female", "female")),
      height = c(178, 157, NA, 174), weight = c(95, 70, 77, NA),
      age = c(23, NA, 21, 22)),
      .expected =  data.frame(
        sex = factor(c("male", "female", "female", "female")),
        height = c(178, 157, 165.5, 174), weight = c(95, 70, 77, 73.5),
        age = c(23, 21.5, 21, 22)))

    expect_matrix_function_equal(ex05Imputation, data.frame(
      sex = factor(c("male", "female", "female", "female")),
      height = c(178, 157, 160, 174), weight = c(95, 70, 77, 69),
      age = c(23, 24, 21, 22)),
      .expected =  data.frame(
        sex = factor(c("male", "female", "female", "female")),
        height = c(178, 157, 160, 174), weight = c(95, 70, 77, 69),
        age = c(23, 24, 21, 22)),
      add.err.info = "Make sure to account for no-occucence cases as well!")

  query <- list()
  result <- list()
  for (sex in c("female", "male")) {
    nummat <- sapply(LETTERS, function(x) rbinom(50, 200, .3))
    mask <- sapply(seq(from = 0, to = 1, length.out = ncol(nummat)), function(x) {
      sample(c(TRUE, FALSE), size = 50, replace = TRUE, prob = c(x, 1 - x))
    })
    mask[1, ] <- FALSE
    nummat[mask] <- NA
    resultmat <- nummat
    resultmat[mask] <- sapply(seq_len(ncol(nummat)), function(i) {
      rep(mean(nummat[, i], na.rm = TRUE), 50)
    })[mask]

    query[[sex]] <- data.frame(sex = factor(rep(sex, 50), c("female", "male")), nummat)
    result[[sex]] <- data.frame(sex = factor(rep(sex, 50), c("female", "male")), resultmat)
  }
  query <- do.call(rbind, query)
  result <- do.call(rbind, result)

  expect_matrix_function_equal(ex05Imputation, query, .expected = result)
  disord <- sample.int(nrow(result))
  expect_matrix_function_equal(ex05Imputation, query[disord, ],
                               .expected = result[disord, ])

})
