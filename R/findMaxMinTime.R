get2dTime <- function(episode){
    time_lt <- 
        lapply(episode@data, 
               function(item){
                   if(length(item) > 1) {
                       if (!is.numeric(item$time))
                           item$time <-
                               as.numeric(as.character(item$time))
                           return(max(item$time))
                   }
               })
    tm <- unlist(time_lt)
    tm
}

#' @export findMinTime
findMinTime <- function(episode) {
    tm <- get2dTime(episode)
    if (is.null(tm))
        return(NULL)
    else
        return(min(tm))
}
#' @export findMaxTime
findMaxTime <- function(episode) {
    tm <- get2dTime(episode)
    if (is.null(tm))
        return(NULL)
    else
        return(max(tm))
}

#' This function is to assign a pseudo time, which is the earliest and latest
#' appeared values of time-series items to admission and discharge time
#' respectively to a record. Please only use this when dealing with the anonymised
#' data, as it modifies the dataset itself. 
#' @export addPseudoTime 
addPseudoTime <- function(record) {
    env <- environment()
    for_each_episode2(record, 
                      function(ep, np, ne) {
                          env$record@patients[[np]]@episodes[[ne]]@admin_icu_time <- 0
                          env$record@patients[[np]]@episodes[[ne]]@discharge_icu_time <- 
                              findMaxTime(ep)
                      })
    return(record)
}
