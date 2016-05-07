#' @importFrom Rcpp evalCpp
#' @useDynLib ccdata 
#' @export reallocateTime
reallocateTime <- function(d, t_discharge, frequency) {
    d_ <- d
    stopifnot(any(names(d) == "time"))
    stopifnot(any(names(d) == "item2d"))
    stopifnot(class(d$time) == "numeric")
    return(reallocateTime_(d_, t_discharge, frequency))
}
