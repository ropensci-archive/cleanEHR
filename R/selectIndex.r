#' get indexing tables for time label, time-wise value, meta data label, and
#' meta data.  
#' @return list of vectors contains time.index, datat.index, meta.index,
#'          datam.index
extractIndexTable <- function() {
    info <- extractInfo()
    MAX_NUM_NHIC <- info$MAX_NUM_NHIC
    # maximum of 4 digits number
    timestamp <- array(0, MAX_NUM_NHIC) # index of NHIC time.
    timevars <- array(0, MAX_NUM_NHIC) # of time-wise values. 
    metalabel <- array(0, MAX_NUM_NHIC) # of meta data label
    metaval <- array(0, MAX_NUM_NHIC) # of meta data value
    simplevar <- array(0, MAX_NUM_NHIC) # 

    timestamp[as.numeric(info$time$idt)] <- 1
    timevars[as.numeric(info$time$id)] <- 1
    metalabel[as.numeric(info$meta$id)] <- 1
    metaval[as.numeric(info$meta$idmeta)] <- 1
    simplevar[as.numeric(info$nontime)] <- 1

    return(data.frame(timestamp, timevars,                  
                      metalabel, metaval, 
                      simplevar))
}


#' here maybe some checking is necessary.
#' @return an selected index
selectIndex<- function(info.index, ids, type){
    #id.num <- as.numeric(removeIdPrefix(ids))
    #stopifnot(max(id.num) < MAX_NUM_NHIC)# maximum of 4 digits number
    id.num <- as.numeric(ids)
    return(which(info.index[[type]][id.num] == 1))
}
