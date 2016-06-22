#' @include ccTable.R

ccTable$methods(
    check.categorical = function() {
        "Check individual entries if they are the in the categories specified
        in conf."
        .self$data_quality$categories <- list()
        .self$data_quality$categories$entry <- 
            .self$torigin[, c("site", "episode_id", "time"), with=FALSE]
        .self$data_quality$categories$episode <- 
            .self$torigin[, .N, by=c("site", "episode_id")]
        setkey(.self$data_quality$categories$entry, "site", "episode_id")
        setkey(.self$data_quality$categories$episode, "site", "episode_id")

        for (i in .self$items) {
            ctg <- .self$conf[[i]][['categories']]
            if (!is.null(ctg)) { 
                # fill the entry table
                .self$data_quality$categories$entry[[i]] <-
                    .self$torigin[[i]] %in% c(names(ctg), "NA", NA)
                # fill the episode table
                epcheck <- 
                    .self$data_quality$categories$entry[, c("site", "episode_id", i), with=FALSE]
                .self$data_quality$categories$episode[[i]] <-
                    epcheck[, all(.SD[[i]]), by=c("site", "episode_id")]$V1
            }
        }
    })


ccTable$methods(
    filter.categorical = function() {
        "Substitute original wrong categorical values with "
        for (iname in names(.self$conf)) {
            this.conf <- .self$conf[[iname]]
            spec <- as.vector(unlist(this.conf)['apply.categorical'])
            .self$spec2function(spec)(iname, .self$data_quality$categories)
        }
    })
