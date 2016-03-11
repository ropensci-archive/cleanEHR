#' S4 class to hold standard IDs such as "NIHR_HIC_ICU_0001"
#' @slot ids single or multiple characters
#' @examples
#' Please use the overloaded constructor to initialise StdId class.
#' selected_tags <- StdId(c("NIHR_HIC_ICU_0001", "NIHR_HIC_ICU_0002")
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
StdId <- function(text) {
    reg <- regexpr("NIHR_HIC_ICU_[0-9]+", text)
    ids <- regmatches(text, reg)
    if (length(text) != length(ids)) 
        stop("StdId: initialisation failure, as the standard ID pattern cannot be found.")
    else
        return(new("StdId", ids=ids))
}

#' convert standard ids to numbers (character) which can be used for indexing.
setGeneric(name="as.number",
           def=function(obj)
           {
               standardGeneric("as.number")
           })


#' convert standard ids to numbers (character) which can be used for indexing.
setMethod(f="as.number",
          signature="StdId", 
          definition=function(obj) 
          {
              d <- as.character(obj@ids)
              if(!all(grepl("NIHR_HIC_ICU_", obj@ids)))# input is code alredy
                  return(obj@ids)
              no.prefix <- 
                  unlist(strsplit(obj@ids, "NIHR_HIC_ICU_"))[seq(2, length(obj@ids) * 2, 2)]
              return(as.character(no.prefix))
          })
