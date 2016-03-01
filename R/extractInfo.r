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

#' retrieve information of the query code/item names from data.checklist
#' @param item.code it can be either item name or NHIC_code, dt_code, or
#'        meta_code
#' @return a vector contains NHIC_code, dt_code, meta_code and row_in_checklist
#' @examples 
#' getItemInfo("Time of death on your unit")
#' getItemInfo("NIHR_HIC_ICU_0001")
getItemInfo <- function(item.code) {
    if (!exists("data.checklist"))
        data("data.checklist")

    if(grepl("NIHR_HIC_ICU_", item.code)){# input is code
        item <- data.checklist$NHICcode == item.code
        dt <- data.checklist$NHICdtCode == item.code
        meta <- data.checklist$NHICmetaCode == item.code
        row.in.list <- which(item | dt | meta)
    }
    else{ # input is item name
        row.in.list <- which(data.checklist$dataItem==item.code)
    }

    if (length(row.in.list) != 1){
        stop("item/NHIC code cannot be found in the list.\n")
    }

    item.info <- c(as.character(data.checklist$dataItem[row.in.list]),
                   as.character(data.checklist$NHICcode[row.in.list]),
                   as.character(data.checklist$NHICdtCode[row.in.list]),
                   as.character(data.checklist$NHICmetaCode[row.in.list]),
                   as.character(data.checklist$Units[row.in.list]),
                   as.character(row.in.list))

    names(item.info) <- c("item", "NHIC_code", "dt_code", 
                          "meta_code", "unit", "row_in_checklist")
    return(item.info)
}

#' get information of a group of code of items and return an array.
getItemsInfo <- function(items.code, var) {
    info_ <- array(NA, length(items.code))
    for (i in seq(items.code))
        info_[i] <- getItemInfo(items.code[i])[var]
    return(info_)
}
