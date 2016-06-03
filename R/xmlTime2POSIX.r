#' convert time from xml table to POSIX format.
#' @param allow If allow is FASLE, it returns error when time format is wrong,
#' however it allows exceptions when "allow" is set to be true, where it
#' returns NA. It is useful when dealing with anonymised data.
#' e.g. 2014-02-01T03:00:00 -> 2014-02-01 03:00:00
#' @export xmlTime2POSIX
xmlTime2POSIX <- function(xml.time, allow=FALSE){
    if (is.null(xml.time))
        return(NA)
    tp <- as.POSIXct(xml.time, "GMT", format="%Y-%m-%dT%H:%M:%S")
    tp[is.na(tp)] <- as.POSIXct(xml.time[is.na(tp)], "GMT", format="%Y-%m-%d")
    if (!allow) {
        if(any(is.na(tp)))
            stop(xml.time[is.na(tp)])
    }
    return(tp)
}
