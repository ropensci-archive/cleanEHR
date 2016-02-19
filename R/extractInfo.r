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
