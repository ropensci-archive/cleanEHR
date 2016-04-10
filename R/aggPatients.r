setGeneric("aggPatients",
           function(record) {
               standardGeneric("aggPatients")
           })

#' Aggregate episodes with the same patient ids (e.g. nhs_number or pas_number)
#' @param record ccRecord
setMethod("aggPatients", 
          c("ccRecord"),
          function(record) {


          })
 
