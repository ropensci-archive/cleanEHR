#' Retrieve one item value at a time from all patients.
#' @param record is a ccRecord object.
#' @param item is a character which can be "nhs_number", "pas_number" and
#' "site_id"
#' @return a vector a all patients selected item. 
#' @usage
#' all.nhs.number <- allPatientsInfo(ccd, "nhs_number")
#' @export allPatientsInfo
allPatientsInfo <- function(record, item) {
    sapply(record@patients, function(x) {
                      ifelse(is.null(slot(x, item)), 
                             stop("slot ", item, 
                                  "cannot be found. Most porbably caused by missing patient"), 
                             slot(x, item))
                      })
}
