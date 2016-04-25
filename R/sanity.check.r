#' @export check.sanity
check.sanity <- function(record, ref) {
    ref.yaml <- yaml.load_file(ref)
    for (item_name in names(ref.yaml)) {
        cat('checking item', item_name, "...\n")
        item <- ref.yaml[[item_name]]
        if (!is.null(item[["range"]])) {
            record <- sanity.range(record, item)
        }
    }
    invisible(record)
}

#' @export init.data.quality
init.data.quality <- function(record) {
    record@data_quality <- lapply(record@patients, 
                                 function(pt) {
                                     lapply(pt@episodes, 
                                            function(ep) {
                                                return(list())
                                            })
                                 })
    return(record)
}



sanity.range <- function(rec, item.ref) {
    e <- new.env()
    nhic_code <- item.ref$NHIC
    e$ptid <- 1
    e$data_quality <- rec@data_quality

    lapply(rec@patients, 
           function(pt) {
               e$epid <- 1
               if (is.null(e$data_quality[e$ptid][[1]]))
                   e$data_quality[[e$ptid]] <- list()
               lapply(pt@episodes, 
                      function(ep) {
                          data <- ep@data[[nhic_code]]
                          if (is.null(nrow(data))) {
                              e$data_quality[[e$ptid]][[e$epid]] <- data
                          }
                          else {
                              if (is.null(e$data_quality[[e$ptid]][e$epid][[1]]))
                                  e$data_qulity[[e$ptid]][[e$epid]] <- list()
                              e$data_quality[[e$ptid]][[e$epid]][[nhic_code]] <- data
                          }
                          e$epid <- e$epid + 1
                      })
               e$ptid <- e$ptid + 1
           })
    rec@data_quality <- e$data_quality
    return(rec)
}
# 0 - unaccepted
# 1 - indeterminable
# 2 - accepted
# 3 - normal

rateRange <- function(data, ref) {
    if (is.null(nrow(data))) { # 1d

    }
    else { # time series, should be able to find item2d

    }

}

