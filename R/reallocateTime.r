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


#' @export reallocateTimeRecord
reallocateTimeRecord <- function(record, delta=0.5) {
    newdata <- for_each_episode(record, 
                                function(e) {
                                    cat('-')
                                    env <- environment()
                                    lapply(e@data, 
                                           function(d) {
                                               if (length(d) > 1) {
                                                   maxtime <- env$e@discharge_icu_time - env$e@admin_icu_time
                                                   maxtime <- as.numeric(maxtime, units="hours")
                                                   return(reallocateTime(d, maxtime, delta))
                                               } else 
                                                   return(d)
                                           })
                                })
    return(ccRecord() + newdata)
}
