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

    ind_dt <- StdId(info$time$idt)
    ind_tvar <- StdId(info$time$id)
    ind_meta <- StdId(info$meta$id)
    ind_mvar <- StdId(info$meta$meta)
    ind_svar <- StdId(info$nontime)

    timestamp[as.numeric(as.number(ind_dt))] <- 1
    timevars[as.numeric(as.number(ind_tvar))] <- 1
    metalabel[as.numeric(as.number(ind_meta))] <- 1
    metaval[as.numeric(as.number(ind_mvar))] <- 1
    simplevar[as.numeric(as.number(ind_svar))] <- 1

    return(data.frame(timestamp, timevars,                  
                      metalabel, metaval, 
                      simplevar))
}


#' here maybe some checking is necessary.
#' @return an selected index
selectIndex<- function(ids, type){
    id.num <- as.numeric(as.number(ids))
    return(ccdata.env$info.index[[type]][id.num] == 1)
}
