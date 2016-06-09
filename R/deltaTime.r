#' @export find.episode.time
find.episode.time <- function(episode) {
    time <- unlist(lapply(episode@data, 
                          function(d) {
                              if (length(d) > 1) 
                                  return(as.character(d$time))
                              else
                                  return(NULL)
                          }), use.names=FALSE)
    if (is.null(time))
        return("NULL")
    else {
        time <- tryCatch(xmlTime2POSIX(time), 
                         error=function(e) {
                             cat("time is not xmlTime format.\n")
                             print(unlist(time))
                             stop()
                         })
        return(c(min(time), max(time)))
    }
}




#' convert calendar time data in a record to delta time comparing to the ICU
#' admission time.
#' @param record ccRecord
#' @param anonymised logical If anonymised is set to be TRUE, then the
#' admission and discharge time will be set as the earliest and latest data stamp
#' in the record.
#' @param units units of delta time, which can be "hours", "mins", "days".
#' @param tdiff if false the delta time will be written in numeric format. 
#' @export deltaTime
deltaTime <- function(record, anonymised=FALSE, units="hours", tdiff=FALSE) {

    # for anonymised data only: 
    # convert hash admin time to the earliest time of the record
    if (anonymised == TRUE) {
        admin_disc_time <<- for_each_episode(record, find.episode.time)
        # NOTE: please try Map here which may look neater. 
        for (p in seq(record@patients)) {
            for(e in seq(record@patients[[p]]@episodes)) {
                record@patients[[p]]@episodes[[e]]@admin_icu_time <- admin_disc_time[[p]][[e]][1]
                record@patients[[p]]@episodes[[e]]@discharge_icu_time <- admin_disc_time[[p]][[e]][2]
            }
        }
    }

    update_time <- function(ep) {
        env <- environment()
        admin_icu_time <- xmlTime2POSIX(ep@admin_icu_time, allow=TRUE)
        if (is.na(admin_icu_time)) {
            return(NULL)
        }
        else {
            lapply(ep@data,
                   function(data) {
                       if (length(data) > 1) {
                           data$time <- difftime(xmlTime2POSIX(data$time), 
                                                 env$admin_icu_time,
                                                 units=units)
                           if (!tdiff)
                               data$time <- as.numeric(data$time)

                       }
                       return(data)
                   })
        }
    }

    record <- ccRecord() + for_each_episode(record, update_time)
    # remove the "NULL" from admin_disc_time, since the NULL episodes are
    # removed in `+` above. 
    admin_disc_time <- admin_disc_time[sapply(admin_disc_time, function(x) x!="NULL")]
    if (anonymised == TRUE) {
        for (p in seq(record@patients)) {
            for(e in seq(record@patients[[p]]@episodes)) {
                record@patients[[p]]@episodes[[e]]@admin_icu_time <- admin_disc_time[[p]][[e]][1]
                record@patients[[p]]@episodes[[e]]@discharge_icu_time <- admin_disc_time[[p]][[e]][2]
            }
        }
    }
    

    return(record)
}
