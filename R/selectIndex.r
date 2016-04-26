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

#'
#' @export .which.datatype
.which.datatype <- function(id) {
  # List with the convertion operations to do for each datatype
  operations <- list('numeric' = as.numeric,
                     'text' = as.character,
                     'date' = as.character, # They are hashed for now
                     'time' = as.character, # They are hashed for now
                     'logical' = as.logical,
                     'list' = as.character,
                     'date/time' = as.character, # They are hashed for now
                     'list / logical' = as.character) # what are they?

  datatype = ccdata.env$ITEM_REF[[id]]$Datatype
  if (!is.null(datatype)){
    if (exists(datatype, operations)){
      return(operations[[datatype]])
    }
  }
  # accounts for not listed or null (eg. when working with dt labels)
  return(as.character)

}
