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
        return(list(admt=min(time), dsct=max(time)))
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
        admdsct <- for_each_episode2(record, find.episode.time)
        for(e in seq(record@episodes)) {
                record@episodes[[e]]@t_admission <- admdsct[[e]]$admt
                record@episodes[[e]]@t_discharge <- admdsct[[e]]$dsct
        }
    }

    update_time <- function(ep) {
        env <- environment()
        t_admission <- xmlTime2POSIX(ep@t_admission, allow=TRUE)
        if (is.na(t_admission)) {
            return(new.episode())
        }
        else {
            eps <- 
                lapply(ep@data,
                       function(data) {
                           if (length(data) > 1) {
                               data$time <- 
                                   difftime(xmlTime2POSIX(data$time), 
                                            env$t_admission,
                                            units=units)
                               if (!tdiff)
                                   data$time <- as.numeric(data$time)

                           }
                           return(data)
                       })
            return(new.episode(eps))
        }
    }


    record <- ccRecord() + for_each_episode2(record, update_time)

    if (anonymised == TRUE) {
        for(e in seq(record@episodes)) {
                record@episodes[[e]]@t_admission <- admdsct[[e]]$admt
                record@episodes[[e]]@t_discharge <- admdsct[[e]]$dsct
            }
    }
    return(record)
}





