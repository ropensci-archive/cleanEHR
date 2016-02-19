#' read csv or xml file and convert to ccdata 
#' @param file input file location.
#' @param file.type specify file type [optional]. 
#' @return ccdata type
getCCData <- function(file, file.type="", ...) {
    if (file.type == "") {
        if (grepl(".csv$", file)) file.type <- "csv"
        if (grepl(".xml$", file)) file.type <- "xml"
    }
    if (file.type == "csv")
        csv2ccd(file, ...)
    else if (file.type == "xml")
        xml2Ccdata(file, ...)
    else
        stop(paste("input file type is unknown,", 
                   "input file can only be either xml or csv format.", 
                   "filename:", file))
}
