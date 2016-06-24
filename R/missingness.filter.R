#' @include ccTable.R

missingness_count <- function(tb) {
    cmplt <- function(vec) {
        length(which(vec!="NA"))/length(vec) * 100
    }
    items <- names(tb)[!names(tb) %in% c("site", "time", "episode_id")]
    flags <- tb[, cmplt(.SD[[items[1]]]), .(episode_id, site)]
    setnames(flags, c('episode_id', 'site', items[1]))
    flags
}

ccTable$methods(
    get.missingness = function() {
        miss_count <- function(tb_) { 
            cmplt <- function(vec) {
                length(which(vec!="NA"))/length(vec) * 100 
            }
            items_ <- names(tb_)[!names(tb_) %in% c("site", "time", "episode_id")]
            flags <- tb_[, cmplt(.SD[[items_[1]]]), .(episode_id, site)]
            setnames(flags, c('episode_id', 'site', items_[1]))
            flags
        }

        .self$dquality[['missingness']] <- .self$torigin[, 1, by=c("episode_id", "site")]
        .self$dquality[['missingness']][, V1:=NULL]
        setkey(.self$dquality[['missingness']], site, episode_id)

        for (i in names(.self$conf)) {
            missconf <- .self$conf[[i]][["missingness"]][["labels"]]
            if(!is.null(missconf)) {
                for (c in seq(missconf)) {
                    col_name <- names(missconf[c])
                    colr <- missconf[[c]]
                    tbq <- selectTable(.self$record, items_opt=i, freq=colr)
                    setkey(tbq, episode_id, site)
                    oldnm <- names(.self$dquality[['missingness']])
                    .self$dquality[['missingness']] <- 
                        merge(.self$dquality[['missingness']], missingness_count(tbq))
                    setnames(.self$dquality[['missingness']], c(oldnm, paste(i, col_name, sep=".")))
                }
            }
        }    
})

ccTable$methods(
    filter.missingness = function(recount=FALSE){
        "filter out the where missingness is too low."
        if (recount || is.null(.self$dquality[['missingness']]) ||
            nrow(.self$dquality[['missingness']]) == 0)
            .self$get.missingness()

        if (is.null(.self$tclean) || nrow(.self$tclean) == 0)
            .self$tclean <- .self$torigin

        thresholds <- 
            unlist(lapply(.self$conf, 
                          function(x) x[["missingness"]][["accept_2d"]]))

        select_index <- rep(TRUE, nrow(.self$dquality[['missingness']]))
        for (nt in names(thresholds))
            select_index <- 
                select_index & as.vector(.self$dquality[['missingness']][, nt, with=FALSE] > thresholds[nt])
        
        .self$dfilter$missingness <- list()
        .self$dfilter$missingness$episode <-
            data.table(.self$dquality$missingness[, c('site', 'episode_id'),
                       with=FALSE], select_index)
})


