.create2dclean <- function(record, config, freq) {
        dt.sofa <- create.cctable(conf=config, record=record, freq=freq)
        dt.sofa$filter.ranges()
        dt.sofa$filter.category()
        dt.sofa$filter.missingness()
        dt.sofa$filter.nodata()
        dt.sofa$apply.filters()
        return(dt.sofa)
}

#' create2dclean
#' @param record ccRecord
#' @param config the full path of the YAML configuration file
#' @param freq table cadence
#' @param nchunks integer number
#' @return A cleaned 2d wide table
#' @export create2dclean
create2dclean <- function(record, config, freq=1, nchunks=1) {

    config <- yaml.load_file(config)
    # number of chunks should not exceed the number of columns
    if (length(config) < nchunks) nchunks <- length(config)
    
    if (nchunks == 1) {
        return(.create2dclean(record, config, freq)$tclean)
    }

    confs <- suppressWarnings(split(config, seq(nchunks)))
    by <- c("site","episode_id", "time")
    
    # create 2d tables in column groups.
    tbclean <- list()
    for (cf in confs) {
        gc()
        dt.sofa <- .create2dclean(record, cf, freq)
        temp <- dt.sofa$tclean
        setkey(temp, "site", "episode_id", "time")
        tbclean[[length(tbclean) + 1]] <- dt.sofa$tclean
    }

    # merge tables
    tbclean_all <- tbclean[[1]][, by, with=F]
    for (i in tbclean) {
        stopifnot(all((tbclean_all$time - i$time) == 0))
        colnames <- names(i[, -by, with=F])
        for(j in colnames) 
            tbclean_all[[j]] <- i[[j]]
    }
    return(tbclean_all)
}
