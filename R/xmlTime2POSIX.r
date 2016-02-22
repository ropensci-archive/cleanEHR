#' convert time from xml table to POSIX format.
#' e.g. 2014-02-01T03:00:00 -> 2014-02-01 03:00:00
xmlTime2POSIX <- function(xml.time){
    return(as.POSIXct(gsub("T", " ", xml.time)))
}
