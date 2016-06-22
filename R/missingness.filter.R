#' @include ccDataTable2.R

missingness_count <- function(tb) {
    cmplt <- function(vec) {
        length(which(vec!="NA"))/length(vec) * 100
    }
    items <- names(tb)[!names(tb) %in% c("site", "time", "episode_id")]
    flags <- tb[, cmplt(.SD[[items[1]]]), .(episode_id, site)]
    setnames(flags, c('episode_id', 'site', items[1]))
    flags
}

ccDataTable$methods(
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

        .self$data_quality[['missingness']] <- .self$torigin[, 1, by=c("episode_id", "site")]
        .self$data_quality[['missingness']][, V1:=NULL]
        setkey(.self$data_quality[['missingness']], episode_id, site)

        for (i in names(.self$conf)) {
            missconf <- .self$conf[[i]][["missingness_2d"]][["labels"]]
            if(!is.null(missconf)) {
                for (c in seq(missconf)) {
                    col_name <- names(missconf[c])
                    colr <- missconf[[c]]
                    tbq <- selectTable(.self$record, items_opt=i, freq=colr)
                    setkey(tbq, episode_id, site)
                    oldnm <- names(.self$data_quality[['missingness']])
                    .self$data_quality[['missingness']] <- 
                        merge(.self$data_quality[['missingness']], missingness_count(tbq))
                    setnames(.self$data_quality[['missingness']], c(oldnm, paste(i, col_name, sep=".")))
                }
            }
        }    
})

ccDataTable$methods(
    filter.missingness = function(recount=FALSE){
        "filter out the where missingness is too low."
        if (recount || is.null(.self$data_quality[['missingness']]) ||
            nrow(.self$data_quality[['missingness']]) == 0)
            .self$get.missingness()

        if (is.null(.self$tclean) || nrow(.self$tclean) == 0)
            .self$tclean <- .self$torigin

        thresholds <- 
            unlist(lapply(.self$conf, 
                          function(x) x[["missingness_2d"]][["accept_2d"]]))

        select_index <- rep(TRUE, nrow(.self$data_quality[['missingness']]))
        for (nt in names(thresholds))
            select_index <- 
                select_index & as.vector(.self$data_quality[['missingness']][, nt, with=FALSE] > thresholds[nt])

        select_table <- .self$data_quality[['missingness']][select_index]
        select_table <- data.table(episode_id=select_table$episode_id,
                                   site=select_table$site)

        .self$tclean <- 
            merge(select_table, .self$tclean, by=c("episode_id", "site"))
})


