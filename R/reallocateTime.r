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



#' getEpisodePeriod
#' @param e episode
#' @return period_length
#' @export getEpisodePeriod
getEpisodePeriod <- function (e, unit="hours") {
    # pseudo delta period, see addPseudoTime()
    if (class(e@discharge_icu_time)[1] == "numeric")
        return(e@discharge_icu_time)

    if (class(e@admin_icu_time)[1] != "POSIXct")
        tadm <- xmlTime2POSIX(as.character(e@admin_icu_time), allow=T)
    else 
        tadm <- e@admin_icu_time
    if (class(e@discharge_icu_time)[1] != "POSIXct")
        tdisc <- xmlTime2POSIX(as.character(e@discharge_icu_time), allow=T)
    else 
        tdisc <- e@discharge_icu_time

    # The failure of POSIX conversion indicates that this episode is either 
    # anonymised or has a missing or incorrect value of discharge or admission
    # time. 
    if (is.na(tadm) || is.na(tdisc))
        period_length <- findMaxTime(e)
    else {
        if (any(is.null(tdisc), is.null(tadm)))
            period_length <- NULL
        else
            period_length <- as.numeric(tdisc - tadm,
                                        units=unit)
    }
    # in cases that tdisc == tadm
    if (!is.null(period_length)) {
        if (period_length == 0)
            period_length <- period_length + 1
    }

    if (is.null(period_length))
        warning("This episode does not have any time series data: ", 
                " episode_id = ", e@episode_id, 
                " nhs_number = ", e@nhs_number, 
                " pas_number = ", e@pas_number,
                " period_length = ", period_length, "\n")


    return(period_length)
}



#' Propagate a numerical delta time interval record.
#' @param record ccRecord
#' @param delta time frequency in hours
#' @details when discharge time and admission time are missing, the latest  and
#' the earliest data time stamp will be used instead.
#' @export reallocateTimeRecord
reallocateTimeRecord <- function(record, delta=0.5) {
    reallocate.episode <- function(e) {
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
    }
    newdata <- for_each_episode(record, reallocate.episode)
    return(ccRecord() + newdata)
}
