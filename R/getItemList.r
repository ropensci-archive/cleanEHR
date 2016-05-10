#' getItemList
#' @export getItemList
getItemList <- function(record, items, freq, item.name=NULL) {
    lt <- for_each_episode(record,
                     function(ep) {
                         if (all(items %in% names(ep@data))) {
                             result <- list()
                             period_length <- getEpisodePeriod(ep)
                             result <- append(result,
                                              itemsToDataFrame(ep, items,
                                                               period_length,
                                                               freq))
                             nlength <- length(result[["time"]])
                             result[["site"]] <- rep(ep@site_id, nlength)
                             result[["episode_id"]] <- rep(ep@episode_id, nlength)
                             return(.simple.data.frame(result))
                         }
                     })
    return(rbindlist(lt))
}

#' @export itemsToDataFrame
itemsToDataFrame <- function(ep, items, period_length, freq) {
    listmatrix <- list()
    time <- seq(0, period_length, freq)

    listmatrix[["time"]] <- time

    for (i in items)
        listmatrix[[i]] <- 
            reallocateTime(ep@data[[i]], period_length, freq)$val
    return(listmatrix)
}
