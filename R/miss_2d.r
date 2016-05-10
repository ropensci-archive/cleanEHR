#' getMissingRate
#' @export get_missing_rate 
get_missing_rate <- function(record, item, frequency, sites=NULL, years=NULL) {
    datalist <- select_data(record, item=item, sites=sites, years=years, propagate=TRUE, 
                            frequency=frequency)
    missing_rate <- sapply(datalist, function(x)
                  length(which(x$val=="NA"))/length(x$val))
}
