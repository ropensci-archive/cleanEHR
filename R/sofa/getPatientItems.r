#' retrieve one column of data from one or multiple patients.
#' @param data2d 
#' @param item
#' @param patient.id 
getPatient1dItem <- function (data1d, item.code, patient.id) {
    return(data1d[[patient.id]][item.code])
}


#' retrieve one column of 2d data from one item of one patients
#' @param data2d
#' @param item
#' @param patient.id
getPatient2dItem <- function (data2d, item.code, patient.id) {
    code.no.prefix <- removeIdPrefix(item.code)
    # TODO: convert to POSIX while reading the xml file.
    tryCatch(
             time <- xmlTime2POSIX(rownames(data2d[[patient.id]])),
             error=function(e){
                 cat("patient.id =", patient.id, "cannot be found!\n")
                 print(e)
             })
    if(code.no.prefix %in% colnames(data2d[[patient.id]])) {
        val <- as.character(data2d[[patient.id]][, code.no.prefix])
        return(data.frame(time, val, stringsAsFactors=FALSE))
    }
    else
        return(NULL)
}
