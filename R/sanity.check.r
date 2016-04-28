#' @export check.sanity
check.sanity <- function(record, ref) {
    if (class(ref) == "list")
        ref.yaml <- ref
    else
        ref.yaml <- yaml.load_file(ref)

    record <- init.data.quality(record)
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
    record@data_quality <- 
        lapply(record@patients, 
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

    item.range <- parse_range(item.ref$range)

    lapply(rec@patients, 
           function(pt) {
               e$epid <- 1
               lapply(pt@episodes, 
                      function(ep) {
                          data <- ep@data[[nhic_code]]
                          if (length(data) == 1) {
                              e$data_quality[[e$ptid]][[e$epid]][[nhic_code]] <- 
                                  rate_range(data, item.range)
                          }
                          else {
                              e$data_quality[[e$ptid]][[e$epid]][[nhic_code]] <- 
                                  rate_range(data$item2d, item.range)

                          }
                          e$epid <- e$epid + 1

                      })
               e$ptid <- e$ptid + 1
           })
    rec@data_quality <- e$data_quality
    return(rec)
}


#' find the lower and upper bound of strings like "10 - 20"
#' @return c(lower_bound, upper_bound)
#' @export yaml_range
yaml_range <- function(ry) {
    if (as.character(ry) == "NULL") 
        return("NULL")
    r <- as.numeric(unlist((strsplit(as.character(ry), '-'))))
    stopifnot(length(r) == 2)
    return(r)
}


#' @export parse_range
parse_range <- function(range.ref) {
    plist <- list()
    for (state in c("indeterminable", "accept", "normal")) {
        plist[[state]] <- yaml_range(range.ref[state])
    }
    return(plist)
}

#' when David's convert_type is merged, we will nolonger need to convert the
#' data type here.
#' @param data vector
#' @param ref result from yaml_range
#' @export rate_range
rate_range <- function(data, ref) {
    data <- as.numeric(as.character(data))
    rate <- rep(0, length(data))
    for(r in seq(length(ref))) {
        if (ref[[r]][1] != "NULL"){
            rate[data >= ref[[r]][1] & data <= ref[[r]][2]] <- r
        }
    }
    return(rate)

}
