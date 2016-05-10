#' convert time from xml table to POSIX format.
#' e.g. 2014-02-01T03:00:00 -> 2014-02-01 03:00:00
#' @export xmlTime2POSIX
xmlTime2POSIX <- function(xml.time){
    tp <- as.POSIXct(xml.time, "GMT", format="%Y-%m-%dT%H:%M:%S")
    tp[is.na(tp)] <- as.POSIXct(xml.time[is.na(tp)], "GMT")
    stopifnot(all(!is.na.POSIXlt(tp)))
    return(tp)
}
