#' @export nakedData
nakedData <- function(record) {
    el <- lapply(record@patients, 
                 function(x) lapply(x@episodes, 
                                    function(e) e))
    return(rapply(el, function(x) x))
}
