#' demographic.table 
#' @name demographic.table
#' @docType methods
#' @param obj  \code{\link{ccRecord2-class}} or \code{\link{ccEpisode-class}}
#' @return return a data.table with all demographic fields of each episode. 
#' @keywords methods
setGeneric("demographic.table", function(obj) {
    standardGeneric("demographic.table")
})

setMethod("demographic.table", signature = c("ccEpisode2"),
          definition = function(obj) {


          })

setMethod("demographic.table", signature = c("ccRecord2"),
          definition = function(obj) {


          })
