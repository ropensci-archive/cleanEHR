#' Provide an index of consequtively repeated items from a a given a binary vector.
#' @param v vector only contains integer 0 or 1
#' @return a vector without repeating.
repeatBinIndex <- function(v) {
    v_ <- c(0, v)[-(length(v)+1)]
    return(v * v_ == 1)
}
