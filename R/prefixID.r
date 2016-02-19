#' add id prefix NIHR_HIC_ICU_0
#' @return string with "NIHR_HIC_ICU_0" as prefix
addIdPrefix <- function(id) {
    return(paste("NIHR_HIC_ICU_", id, sep=""))
}



#' remove the prefix of id, i.e. NIHR_HIC_ICU_0xxx -> xxx
#' @return character of numeric ids, e.g. "0123"
removeIdPrefix <- function(id) {
    no.prefix <- unlist(strsplit(as.character(id), 
                    "NIHR_HIC_ICU_"))[seq(2, length(id) * 2, 2)]
    if(any(is.na(as.numeric(no.prefix))))
       stop(id, "\nid should look NIHR_HIC_ICU_0xxx\n")
    return(as.character(no.prefix))
}
