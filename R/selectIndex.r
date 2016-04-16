#' get indexing tables for time label, time-wise value, meta data label, and
#' meta data.  
#' @return list of vectors contains time.index, datat.index, meta.index,
#'          datam.index
extractIndexTable <- function() {
    info <- extractInfo()

    checklist <- list()
    for(i in seq(info$nontime))
        checklist[[info$nontime[i]]] <- "item1d"
    for(i in seq(info$time$idt))
        checklist[[info$time$idt[i]]] <-"time"
    for(i in seq(info$time$id))
        checklist[[info$time$id[i]]] <- "item2d"
    for(i in seq(info$meta$meta))
        checklist[[info$meta$meta[i]]] <- "meta"
    
    return(checklist)
}

#' this one should no longer be useful.
#' here maybe some checking is necessary.
#' @return an selected index
selectIndex<- function(ids, type){
    id.num <- as.numeric(as.number(ids))
    return(ccdata.env$info.index[[type]][id.num] == 1)
}

#'
#' @export .which.type
.which.type <- function(id) {
    return(ccdata.env$checklist[[id]])
}
