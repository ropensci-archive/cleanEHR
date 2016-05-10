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


findMaxTime <- function(episode) {
    maxlist <- lapply(episode@data, 
                      function(item){
                          if(length(item) > 1)
                              return(max(item$time))
                      })
    return(max(unlist(maxlist)))
}

findMinTime <- function(episode) {
    maxlist <- lapply(episode@data, 
                      function(item){
                          if(length(item) > 1)
                              return(max(item$time))
                      })
    return(max(unlist(maxlist)))
}



#' getEpisodePeriod
#' @param e episode
#' @return period_length
#' @export getEpisodePeriod
getEpisodePeriod <- function (e, unit="hours") {
    if (class(e@admin_icu_time)[1] != "POSIXct")
        tadmin <- xmlTime2POSIX(e@admin_icu_time)
    else 
        tadmin <- e@admin_icu_time

    if (class(e@discharge_icu_time)[1] != "POSIXct")
        tdisc <- xmlTime2POSIX(e@discharge_icu_time)
    else 
        tdisc <- e@discharge_icu_time

    if (is.na(tadmin) || is.na(tdisc))
        period_length <- findMaxTime(e)
    else
        period_length <- as.numeric(tdisc - tadmin,
                                    units=unit)
    if (is.na(period_length))
        stop("wrong period_length at episode: ", 
             "\nepisode_id =", e@episode_id, 
             "\nnhs_number =", e@nhs_number, 
             "\npas_number =", e@pas_number,
             "\nperiod_length =", period_length, "\n")


    return(period_length)
}




#' Propagate a numerical delta time interval record.
#' @param record ccRecord
#' @param delta time frequency in hours
#' @details when discharge time and admission time are missing, the latest  and
#' the earliest data time stamp will be used instead.
#' @export reallocateTimeRecord
reallocateTimeRecord <- function(record, delta=0.5) {
    newdata <- for_each_episode(record, 
                                function(e) {
                                    env <- environment()
                                    # make sure admin and disc time is correct
                                    period_length <- getEpisodePeriod(e)

                                    # calling reallocateTime for each data item
                                    lapply(e@data, 
                                           function(d) {
                                               if (length(d) > 1) {
                                                   return(reallocateTime(d, env$period_length, delta))
                                               } else 
                                                   return(d)
                                           })
                                })
    return(ccRecord() + newdata)
}
