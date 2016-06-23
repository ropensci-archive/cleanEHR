#' @include ccTable.R

ccTable$methods(
    check.categorical = function() {
        "Check individual entries if they are the in the categories specified
        in conf."
        .self$dfilter$categories <- list()
        .self$dfilter$categories$entry <- 
            .self$torigin[, c("site", "episode_id", "time"), with=FALSE]
        .self$dfilter$categories$episode <- 
            .self$torigin[, .N, by=c("site", "episode_id")]
        setkey(.self$dfilter$categories$entry, "site", "episode_id")
        setkey(.self$dfilter$categories$episode, "site", "episode_id")

        for (i in .self$items) {
            ctg <- .self$conf[[i]][['categories']]
            if (!is.null(ctg)) { 
                # fill the entry table
                .self$dfilter$categories$entry[[i]] <-
                    .self$torigin[[i]] %in% c(names(ctg), "NA", NA)
                # fill the episode table
                epcheck <- 
                    .self$dfilter$categories$entry[, c("site", "episode_id", i), with=FALSE]
                .self$dfilter$categories$episode[[i]] <-
                    epcheck[, all(.SD[[i]]), by=c("site", "episode_id")]$V1
            }
        }
    })


ccTable$methods(
    filter.categorical = function() {
        "Substitute original wrong categorical values with "
        for (iname in names(.self$conf)){
            .self$spec2function(iname, 
                                'categorical')(iname, 
                                .self$dfilter$categories)
        }
    })
