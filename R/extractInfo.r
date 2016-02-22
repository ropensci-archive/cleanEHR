#' give id number from NHIC code like "NIHR_HIC_ICU_xxxx"
#' @param nhic NHIC code
whichIsCode <- function(nhic) {
    return(grepl(nhic, pattern="[0-9][0-9][0-9][0-9]"))
}

#' extract information from data.checklist
#' @return list of time [data.frame(id, idt)], meta [data.frame(id, idmeta)], 
#'         nontime [numeric], MAX_NUM_NHIC
extractInfo <- function() {
    data("data.checklist")
    index.time <- whichIsCode(data.checklist$NHICdtCode) 
    index.meta <- whichIsCode(data.checklist$NHICmetaCode)
    time.list <- data.frame(id=removeIdPrefix(data.checklist$NHICcode[index.time]), 
                            idt=removeIdPrefix(data.checklist$NHICdtCode[index.time]),
                            stringsAsFactors=FALSE)
    meta.list <- data.frame(id=removeIdPrefix(data.checklist$NHICcode[index.meta]), 
                            idmeta=removeIdPrefix(data.checklist$NHICmetaCode[index.meta]), 
                            stringsAsFactors=FALSE)
    nontime<- removeIdPrefix(data.checklist$NHICcode[!index.time])
    # get all ids which should be the assemble of NHICcode and NHICmetaCode
    all.ids <- c(meta.list$idmeta,
                 removeIdPrefix(data.checklist$NHICcode))
    if (any(duplicated(all.ids)))
        stop("data.checklist.RData error! meta data code and NHICcode are overlaped")
    return(list(time=time.list, meta=meta.list, nontime=nontime,
                MAX_NUM_NHIC=max(c(all.ids, time.list$idt))))
}


#' @param item item name
#' @return a vector contains NHIC_code, dt_code, meta_code and row_in_checklist
#' @examples 
#' whichId("Time of death on your unit")
whichId <- function(item) {
    if (!exists("data.checklist"))
        data("data.checklist")

    row.in.list <- which(data.checklist$dataItem==item)
    if (length(row.in.list) != 1){
        cat("item list\n============\n")
        print(data.checklist$dataItem)
        stop("item cannot be found in the list.\n")
    }

    ids <- c(as.character(data.checklist$dataItem[row.in.list]),
             as.character(data.checklist$NHICcode[row.in.list]),
             as.character(data.checklist$NHICdtCode[row.in.list]),
             as.character(data.checklist$NHICmetaCode[row.in.list]),
             as.character(row.in.list))

    names(ids) <- c("item", "NHIC_code", "dt_code", "meta_code", "row_in_checklist")
    return(ids)
}

#' @param nhic.code it can be NHIC_code, dt_code or meta_code
#' @return a vector corresponding to query code, which contains NHIC_code, 
#'         dt_code, meta_code and row_in_checklist
whichItem <- function(code) {
    if (!exists("data.checklist"))
        data("data.checklist")
    item <- data.checklist$NHICcode == code
    dt <- data.checklist$NHICdtCode == code
    meta <- data.checklist$NHICmetaCode == code
    
    row.in.checklist <- which(item | dt | meta)

    if (length(row.in.checklist) != 1)
        stop("code cannot be found in the list.\n")

    item <- c(as.character(data.checklist$dataItem[row.in.checklist]),
              as.character(data.checklist$NHICcode[row.in.checklist]),
              as.character(data.checklist$NHICdtCode[row.in.checklist]),
              as.character(data.checklist$NHICmetaCode[row.in.checklist]),
              as.character(row.in.checklist))
    names(item) <- c("item", "NHIC_code", "dt_code", "meta_code", "row_in_checklist")
    return(item)
}
