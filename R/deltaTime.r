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





#' @export deltaTime
deltaTime <- function(record) {
    time1 <-
        xmlTime2POSIX(record@patients[[1]]@episodes[[1]]@admin_icu_time)

    # for anonymised data only: 
    # convert hash admin time to the earliest time of the record
    if (is.na(time1)) {
        admin_disc_time <- for_each_episode(record, find.episode.time)
        # NOTE: please try Map here which may look neater.
        for (p in seq(record@patients)) {
            for(e in seq(record@patients[[p]]@episodes)) {
                record@patients[[p]]@episodes[[e]]@admin_icu_time <- 
                    gsub(" ", "T", as.character(admin_disc_time[[p]][[e]][1]))
                record@patients[[p]]@episodes[[e]]@discharge_icu_time <- 
                    gsub(" ", "T",   as.character(admin_disc_time[[p]][[e]][2]))
            }
        }
    }

    for(p in seq(record@patients)) {
        for(e in seq(record[p])) {
            for(d in seq(record[p, e]@data)) {
                new <- record[p, e, d]
                if (length(new) > 1) {
                    new$time <- xmlTime2POSIX(new$time) - 
                        xmlTime2POSIX(record[p, e]@admin_icu_time)
                    setValue(record, p, e, d, new)
                }
            }
        }

    }
    return(record)

}
