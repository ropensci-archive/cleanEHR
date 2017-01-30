.create2dclean <- function(record, config, freq) {
        dt.sofa <- create.cctable(rec=record, conf=config, freq=freq)
        dt.sofa$filter.ranges()
        dt.sofa$filter.category()
        dt.sofa$filter.missingness()
        dt.sofa$filter.nodata()
        dt.sofa$apply.filters()
        return(dt.sofa)
}

#' Create a 2D wide clean table - low memory
#' 
#' The cleaning process is specified by the YAML configuration. All the filters
#' presented in the configuration will be applied. It returns only the cleaned
#' data. However all the data quality information will be lost. This function
#' is useful when the memory is not sufficiently enough to hold all the
#' information. 
#' @param record ccRecord
#' @param config the full path of the YAML configuration file
#' @param freq table cadence
#' @param nchunks integer number. The larger the nchunks the less memory
#' requirement. 
#' @return A cleaned 2d wide table
#' @export create2dclean
create2dclean <- function(record, config, freq=1, nchunks=1) {
    if (is.character(config))
        config <- yaml.load_file(config)

    stopifnot(nchunks > 0 & nchunks < record@nepisodes)
    
    if (nchunks == 1)
        return(.create2dclean(record, config, freq)$tclean)

    op.seq <- round(seq(1, record@nepisodes + 1, length.out=nchunks + 1))

    tclean <- list()

    for (i in seq(length(op.seq) - 1)) {
        rc <- record[seq(op.seq[i], op.seq[i+1] - 1)]
        tclean[[i]] <- .create2dclean(rc, config, freq)$tclean
        gc()
    }
    
    tclean <- rbindlist(tclean, fill=TRUE)
    return(tclean)
}
