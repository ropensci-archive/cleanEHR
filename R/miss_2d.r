#' getMissingRate
#' @export get_missing_rate 
get_missing_rate <- function(record, item, freq, sites=NULL, years=NULL) {
    datalist <- select_data(record, item=item, sites=sites, years=years, propgt=TRUE, 
                            freq=freq)
    missing_rate <- sapply(datalist, function(x)
                  length(which(x$val=="NA"))/length(x$val))
    missing_rate
}
