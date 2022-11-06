
context("exercises in 04_exercise_control_structures.R")

test_that("Exercise 01: Semantic Network", {
  exerciseTest("4.1")
  
  knowledge = data.frame(entity = c("cat", "dog", "dog", "mammal", "felix the cat"),
     is.a = c("mammal", "mammal", "man's best friend", "animal", "cat"), stringsAsFactors = FALSE)
  expect_vector_function_equal(ex01SemanticNetwork,
                               knowledge = knowledge, special = "cat", general = "animal",
                               .expected = TRUE, class = "logical", length = 1)
  
  specialsT <- c("cat", "felix the cat", "dog", "bird")
  generalsT <- c("animal", "animal", "dog", "bird")
  specialsF <- c("cat", "animal", "felix the cat", "bird")
  generalsF <- c("dog", "cat", "dog", "animal")
  
  for (i in seq_along(specialsT)) {
    expect_vector_function_equal(ex01SemanticNetwork,
                                 knowledge = knowledge, special = specialsT[i], general = generalsT[i],
                                 .expected = TRUE)
    
    expect_vector_function_equal(ex01SemanticNetwork,
                                 knowledge = knowledge, special = specialsF[i], general = generalsF[i],
                                 .expected = FALSE)
  }

  # same knowledge database, but more (redundant) entries

  knowledge <- data.frame(entity = c("cat", "dog", "mammal", "felix the cat", "cat", "dog", "felix the cat"),
    is.a = c("mammal", "mammal", "animal", "cat", "animal", "animal", "mammal"), stringsAsFactors = FALSE)
  
  for (i in seq_along(specialsT)) {
    expect_vector_function_equal(ex01SemanticNetwork,
                                 knowledge = knowledge, special = specialsT[i], general = generalsT[i],
                                 .expected = TRUE)
    
    expect_vector_function_equal(ex01SemanticNetwork,
                                 knowledge = knowledge, special = specialsF[i], general = generalsF[i],
                                 .expected = FALSE)
  }
  
  knowledge <- data.frame(entity = "man",
    is.a = "mortal", stringsAsFactors = FALSE)

  expect_vector_function_equal(ex01SemanticNetwork,
                               knowledge = knowledge, special = "man", general = "man",
                               .expected = TRUE)
  expect_vector_function_equal(ex01SemanticNetwork,
                               knowledge = knowledge, special = "man", general = "mortal",
                               .expected = TRUE)
  expect_vector_function_equal(ex01SemanticNetwork,
                               knowledge = knowledge, special = "mortal", general = "mortal",
                               .expected = TRUE)
  expect_vector_function_equal(ex01SemanticNetwork,
                               knowledge = knowledge, special = "mortal", general = "man",
                               .expected = FALSE)

  knowledge <- data.frame(
      entity = c("hovercraft", "hovercraft", "land vehicle", "watercraft", "boat", "vehicle", "art"),
      is.a = c("land vehicle", "watercraft", "vehicle", "vehicle", "watercraft", "human made", "human made"),
      stringsAsFactors = FALSE)
  specialsT <- c("boat", "hovercraft", "hovercraft", "art", "hovercraft", "hovercraft")
  generalsT <- c("boat", "watercraft", "human made", "human made", "land vehicle", "hovercraft")
  specialsF <- c("hovercraft", "boat", "art", "land vehicle", "land vehicle", "name")
  generalsF <- c("boat", "land vehicle", "land vehicle", "watercraft", "hovercraft", "differentName")
  
  for (i in seq_along(specialsT)) {
    expect_vector_function_equal(ex01SemanticNetwork,
                                 knowledge = knowledge, special = specialsT[i], general = generalsT[i],
                                 .expected = TRUE)
    
    expect_vector_function_equal(ex01SemanticNetwork,
                                 knowledge = knowledge, special = specialsF[i], general = generalsF[i],
                                 .expected = FALSE)
  }
})

test_that("Exercise 02: Cellular Automaton", {
  exerciseTest("4.2")
  
  expect_matrix_function_equal(ex02CellularAutomaton,
                               initial.state = c(0, 0, 0, 1, 0, 0, 0),
                               steps = 4,
                               .expected = rbind(c(0, 0, 0, 1, 0, 0, 0),
                                                c(0, 0, 1, 1, 1, 0, 0),
                                                c(0, 1, 1, 0, 0, 1, 0),
                                                c(1, 1, 0, 1, 1, 1, 1)),
                               class = "matrix")

  expect_matrix_function_equal(ex02CellularAutomaton,
                               initial.state = 1,
                               steps = 5,
                               .expected = rbind(1, 1, 1, 1, 1)
                               )

  expect_matrix_function_equal(ex02CellularAutomaton,
                               initial.state = c(0, 0, 1),
                               steps = 5,
                               .expected = rbind(c(0, 0, 1),
                                                c(0, 1, 1),
                                                c(1, 1, 0),
                                                c(1, 0, 1),
                                                c(1, 0, 1))
  )

output <- expect_matrix_function_equal(ex02CellularAutomaton,
                               initial.state = c(0, 0, 1, 0),
                               steps = 10,
                               .expected = NULL,
                               .output.result = TRUE)

  called <- paste0(as.character(substitute(ex02CellularAutomaton)),
                 " was called with the following arguments: \n",
                 sprintf("initial.state: \n %s \n steps: %s", printToText(c(0, 0, 1, 0)),
                         10))
  classCheck(output, "matrix", called)
  expect_equal(nrow(output), 10, info = paste0("\n", sprintf("Your function should return a matrix with nrow = 10, but returns one with nrow = %s \n", 
                                                       nrow(output)) , called))
  
  if("matrix" %in% class(output) && nrow(output == 10)) {
    expect_equal(output[10, ], c(1, 0, 1, 0), info = paste0(
    sprintf("Your function returns a matrix with the last row of: \n %s \n but it should be: \n %s \n",
            printToText(output[10, ]), printToText(c(1, 0, 1, 0))), called))
  }           

  shouldBeOne <- function(above) {
    above[[1]] && !(above[[2]] || above[[3]]) ||
      !above[[1]] && (above[[2]] || !above[[2]] && above[[3]])
  }

  for (i in seq_len(2)) {
    init <- sample(c(0, 1), size = 20, replace = TRUE)
    result <- expect_matrix_function_equal(ex02CellularAutomaton,
                                           initial.state = init,
                                           steps = 40,
                                           .expected = NULL,
                                           .output.result = TRUE)
    called <- paste0(as.character(substitute(ex02CellularAutomaton)),
                     " was called with the following arguments: \n",
                     sprintf("initial.state: \n %s \n steps: %s", printToText(init),
                             40))
    classCheck(result, "matrix", called)
    expect_equal(nrow(result), 40, info = paste0("\n", sprintf("Your function should return a matrix with nrow = 40, but returns one with nrow = %s \n", 
                                                               nrow(result)) , called))
    expect_equal(ncol(result), 20, info = paste0("\n", sprintf("Your function should return a matrix with ncol = 20, but returns one with ncol = %s \n", 
                                                               ncol(result)) , called))
    
    if("matrix" %in% class(result) && nrow(result) == 40 && ncol(result) == 20) {
      
      expect_equal(result[2:40, ], expect_matrix_function_equal(ex02CellularAutomaton,
                                                                initial.state = result[2, ],
                                                                steps = 39,
                                                                .expected = NULL,
                                                                .output.result = TRUE),
                   info = paste0("\n", 
              sprintf("Calling your function again with line %s of the original output, doesn't result in the same result: \n
result[%s:40, ] is not the same as ex02CellularAutomaton(result[%s, ], 39) \n", 
                                               2, 2, 2) , paste(" \n result was made like this: \n", called)))
      expect_equal(result[3:40, ], expect_matrix_function_equal(ex02CellularAutomaton,
                                                                initial.state = result[3, ],
                                                                steps = 38,
                                                                .expected = NULL,
                                                                .output.result = TRUE),
                   info = paste0("\n", 
                                 sprintf("Calling your function again with line %s of the original output, doesn't result in the same result: \n
result[%s:40, ] is not the same as ex02CellularAutomaton(result[%s, ], 39) \n", 
                                         3, 3, 3) , paste("\n result was made like this: \n", called)))
      
      result <- cbind(0, result, 0)
      for (col in seq_len(4)) {
        for (row in seq_len(3)) {
          above <- result[row, col:(col + 2)]
          .expected <- as.numeric(any(sapply(list(c(1, 0, 0), c(0, 1, 1), c(0, 1, 0), c(0, 0, 1)), identical, above)))
          expect_equal(result[row + 1, col + 1], .expected,
                       info = paste0("\n", 
                      sprintf("There are flaws in the logic of your function. 
                              \n For example: The value of row %s, column %s should be %s based on the 3 relevant values in the row above: \n %s \n but is %s .\n",
                              row + 1, col + 1, .expected, printToText(above), result[row + 1, col + 1]),
                      "The function was called like this: \n", called))
        }
      }
    }
  }

})