#' S4 class to hold standard IDs such as "NIHR_HIC_ICU_0001"
#' @slot ids single or multiple characters
#' @examples
#' \donttest{
#' selected_tags <- StdId(c("NIHR_HIC_ICU_0001", "NIHR_HIC_ICU_0002")
#' }
StdId <- setClass ("StdId",
                   slots = c(ids="vector"), 
                   validity=function(object)
                   {
                       reg <- regexpr("NIHR_HIC_ICU_[0-9]+", object@ids)
                       ids <- regmatches(object@ids, reg)
                       if (length(object@ids) != length(ids)) 
                           return("initialisation failure, as the standard ID pattern cannot be found.")
                       else
                           object@ids = ids
                       return(TRUE)
                   })


#' constructor of StdId class
#' @export StdId
StdId <- function(text) {
    reg <- regexpr("NIHR_HIC_ICU_[0-9]+", text)
    ids <- regmatches(text, reg)
    if (length(text) != length(ids) | length(text) == 0) 
        stop("StdId: initialisation failure, as the standard ID pattern cannot be found.")
    else
        return(new("StdId", ids=ids))
}


#' convert standard ids to numbers (character) which can be used for indexing.
#' @export as.number
as.number <- function(obj) {
    d <- as.character(obj@ids)
    if(!all(grepl("NIHR_HIC_ICU_", obj@ids)))# input is code alredy
        return(obj@ids)
    no.prefix <- 
        unlist(strsplit(obj@ids, "NIHR_HIC_ICU_"))[seq(2, length(obj@ids) * 2, 2)]
    return(as.character(no.prefix))
}

#'
#' @export .as.number
.as.number <- function(code) {
    return(as.numeric(strsplit(code, "NIHR_HIC_ICU_")[[1]][2]))
}


all.nhic.code <- function(cls) {
        data.checklist[data.checklist$Classification1 == cls,"NHICcode"]
}

code2stname <- function(code) {
    ccdata.env$code2stname.dict[code]
}

stname2code <- function(stname) {
    ccdata.env$stname2code.dict[stname]
}

short2longname <- function(stname) {
    longname <- array("NULL", length(stname))
    for (i in seq_along(stname))
        longname[i] <- ccdata.env$ITEM_REF[[stname2code(stname[i])]]$dataItem
    return(longname)
}
