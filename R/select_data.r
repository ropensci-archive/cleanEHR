#' Retrieve one item at a time from the ccRecord
#' @param record ccRecord
#' @param item_id NHIC code
#' @export readOneItem
readOneItem <- function(record, item_id, convert_type=NULL, as.POSIX=FALSE,
                        unlist=TRUE) {
    StdId(item_id)
    short_name <- ccdata.env$ITEM_REF[['item_id']]$shortName
    if (is.null(short_name))
        short_name <- "NULL"
    env <- new.env()
    env$tb <- list()
    lapply(record@patients, 
           function(x) {
               lapply(x@episodes, 
                      function(x) {
                          pos <- length(env$tb) + 1
                          if (as.POSIX != FALSE) {
                              x@data[[item_id]]$time <-
                                  xmlTime2POSIX(x@data[[item_id]]$time)
                          }
                          if (!is.null(convert_type)) {
                              if (length(x@data[[item_id]]) == 1) {
                                  x@data[[item_id]] <-
                                      convert_type(as.character((x@data[[item_id]])))
                              }
                              else {
                                  x@data[[item_id]]$item2d <-
                                      convert_type(as.character(x@data[[item_id]]$item2d))
                              }
                          }

                          if (length(x@data[[item_id]]) == 1) {
                              env$tb[[pos]] <- data.frame(nhs_number=x@nhs_number, 
                                                          pas_number=x@pas_number,
                                                          episode_id=x@episode_id, 
                                                          site_id=x@site_id,
                                                          item=item_id,
                                                          short_name=short_name,
                                                          time=NA,
                                                          val=x@data[[item_id]])
                          }
                          else if (length(x@data[[item_id]]) == 2) {
                              nr <- nrow(x@data[[item_id]])
                              env$tb[[pos]] <-
                                  data.table(nhs_number = rep(x@nhs_number, nr), 
                                             pas_number = rep(x@pas_number, nr),  
                                             episode_id = rep(x@episode_id, nr),
                                             site_id    = rep(x@site_id, nr),
                                             item       = rep(item_id, nr),
                                             short_name = rep(short_name, nr),
                                             time       = x@data[[item_id]]$time,
                                             val        = x@data[[item_id]]$item2d)
                          }
                      })
           })
    if (unlist == TRUE) {
        return(rbindlist(env$tb))
    }
    return(env$tb)
}

#' @export readItems
readItems <- function(record, item_ids, ...) {
    if (any(duplicated(item_ids)))
        stop("item ids should not be duplicated.")
    tt <- list()
    for (id in item_ids) 
        tt[[id]] <- readOneItem(record, id, unlist=TRUE)
    return(rbindlist(tt))
}
