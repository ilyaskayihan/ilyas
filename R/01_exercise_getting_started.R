# Solve these exercises by filling in the function bodies, and try to read up on
# things that you didn't know or surprise you.

# write a function that divides x by y
# x and y are both single numbers.
ex01Divide <- function(x, y) {
  x / y
}

# write a function that divides x by y
# x and y are numeric vectors. Does this function need to
# differ from the one above?
ex02DivideVectors <- function(x, y) {
  if(length(x) == length(y)) {
  x / y
 } else {
    print("Not same length")
  }
}

# You have to pay attention that the length of the vector is the same

# write a function that checks if a "logical", a "numeric", or a "character" vector or something else.
# the function should return "L" if the input is logical, "N" if it is numeric, "C" for characters, and otherwise "X".
ex03VectorType <- function(x) {
  if (typeof(x) == "logical") {
    return("L")
  } else {
    if (typeof(x) == "numeric") {
      return("N")
    } else {
      if (typeof(x) == "character") {
        return("C")
      } else {
        return("X")
      }
    }
  }
}
ex03VectorType("Test")
# write a function that takes a vector of integers and returns all numbers that are even.
# an empty vector should be returned when no even numbers are found.
ex04Even <- function(x) {
  subset(x, x %% 2 == 0)
}

