#' Helper functions to add the numbers on an array in a
#' cummulative fashion using Reduce
#' extracted from: http://adv-r.had.co.nz/Functionals.html
add <- function(x, y, na.rm = FALSE) {
  if (na.rm && (is.na(x) || is.na(y))) rm_na(x, y, 0) else x + y
}

c_add <- function(xs, na.rm = FALSE) {
  Reduce(function(x, y) add(x, y, na.rm = na.rm), xs, accumulate = TRUE)
  }

#' Produces an index label that doesn't change for consecutive times
#' @param array is a boolean array
#' @return a vector with indices for when there is changes
#' @usage
#' indices <- consecutivetime(c(T, F, T, T))
#' @export consecutivetime 
consecutivetime <- function(array){
  l = length(array)
  ## operate the array against itself shifted on position
  ## by an 'and' and negated. A FALSE is added at the begining
  ## to contain the same number of elements and start counting
  ## from it.
  newarray <- c(F, ! ( array[2:l] & array[1:l-1]))
  ## added the array with a cummulative operation to obtain the
  ## index change when there's a TRUE
  index <- c_add(newarray)
  ## added one as otherwise it starts from 0.
  return(index + 1)
  }
