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

#'
#' @export .which.type
.which.type <- function(id) {
    return(ccdata:::checklist[[id]])
}

#' 
#' @export .which.datatype
.which.datatype <- function(id) {
  # List with the conversion operations to do for each data type
  operations <- list('numeric' = as.numeric,
                     'text' = as.character,
                     'date' = as.character, # They are hashed for now
                     'time' = as.character, # They are hashed for now
                     'logical' = as.logical,
                     'list' = as.character,
                     'date/time' = as.character, # They are hashed for now
                     'list / logical' = as.character) # what are they?

  datatype = ccdata:::ITEM_REF[[id]]$Datatype
  if (!is.null(datatype)){
    if (exists(datatype, operations)){
      return(operations[[datatype]])
    }
  }
  # accounts for not listed or null (eg. when working with dt labels)
  return(as.character)
}


#' Convert item data to its corresponding data type.
#' @param id NHIC code of the data
#' @param vals The vector of values of the item.
#' @return vector values in its corresponding data type
which.datatype <- function(id, vals) {
}
