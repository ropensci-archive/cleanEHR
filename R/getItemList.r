#' getItemList
#' @param record ccRecord
#' @param items_obg obligatory items that is obligatory; Any episode that doesn't contain
#' item in this vector will be removed.
#' @param items_opt optional items
#' @return data.table
#' @export selectTable
selectTable <- function(record, items_opt=NULL, items_obg=NULL, freq, item.name=NULL) {
    all_items <- c(items_opt, items_obg)
    if (is.null(all_items))
        stop('both items_opt and items_obg are NULL')

    env <- environment()
    lt <- list()
    for_each_episode(record,
                     function(ep) {
                         if (all(items_obg %in% names(ep@data))) {
                             result <- list()
                             period_length <- getEpisodePeriod(ep)
                             # getEpisodePeriod will return NULL when no 2D
                             # data been found. 
                             if (!is.null(period_length)) { 
                                 result <- append(result,
                                                  itemsToDataFrame(ep, all_items,
                                                                   period_length,
                                                                   freq))
                                 nlength <- length(result[["time"]])
                                 result[["site"]] <- rep(ep@site_id, nlength)
                                 result[["episode_id"]] <- rep(ep@episode_id, nlength)
                                 env$lt[[length(lt) + 1]]<- .simple.data.frame(result)
                             }
                         }
                     })
    dt <- rbindlist(lt)
    
    if (!is.null(item.name))
        setnames(dt, c("time", item.name, "site", "episode_id"))
    return(dt)
}

#' @export itemsToDataFrame
itemsToDataFrame <- function(ep, items, period_length, freq) {
    listmatrix <- list()
    time <- seq(0, period_length, freq)

    listmatrix[["time"]] <- time

    for (i in items) {
        if ("time" %in% names(ep@data[[i]]))
            listmatrix[[i]] <- reallocateTime(ep@data[[i]], period_length, freq)$val
        else
            listmatrix[[i]] <- rep("NA", length(time))
    }
    return(listmatrix)
}
