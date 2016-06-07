#' this is ref example
#' @import data.table
#' @include ccdata.r
#' @export ccDataTable2
ccDataTable2 <- setRefClass("ccDataTable2", 
                            fields=c(conf="list",
                                     torigin="data.table", 
                                     tclean="data.table",
                                     record="ccRecord", 
                                     missingness="data.table",
                                     range="data.table"))
ccDataTable2$methods(
show = function() {
    cat("torigin:\n------\n")
    print(.self$torigin)
    cat("tclean:\n------\n")
    print(.self$tclean)
})

ccDataTable2$methods(
create.table = function(freq){
    "Create a table contains the selected items in the conf with a given
    frequency (in hour)"
    items <- names(.self$conf)
    .self$torigin <- selectTable(record=record, items_opt=items, 
                               freq=freq)
    .self$tclean <- .self$torigin
})

ccDataTable2$methods(
filter.missingness = function(recount=FALSE){
    "filter out the where missingness is too low."
    if (recount || is.null(.self$missingness) ||
       nrow(.self$missingness) == 0)
        .self$count.missingness()

    if (is.null(.self$tclean) || nrow(.self$tclean) == 0)
        .self$tclean <- .self$torigin

    thresholds <- 
        unlist(lapply(.self$conf, 
                      function(x) x[["missingness_2d"]][["miss_acceptance"]]))
    
    # how to make functions data.table awared? so that we can avoid these ugly
    # indexing.
    select_index <- rep(TRUE, nrow(.self$missingness))
    for (nt in names(thresholds))
        select_index <- 
            select_index & as.vector(.self$missingness[, nt, with=FALSE] > thresholds[nt])

    select_table <- .self$missingness[select_index]
    select_table <- data.table(episode_id=select_table$episode_id,
                               site=select_table$site)
    
    .self$tclean <- 
        merge(select_table, .self$tclean, by=c("episode_id", "site"))
})


ccDataTable2$methods(
count.missingness = function() {
    miss_count <- function(tb) { 
        cmplt <- function(vec) {
            length(which(vec!="NA"))/length(vec) * 100 
        }
        items <- names(tb)[!names(tb) %in% c("site", "time", "episode_id")]
        stopifnot(length(items)==1) 
        flags <- tb[, cmplt(.SD[[items[1]]]), .(episode_id, site)]
        setnames(flags, c('episode_id', 'site', items))
        flags
    }
    
    .self$missingness <- .self$torigin[, 1, by=c("episode_id", "site")]
    .self$missingness[, V1:=NULL]
    setkey(.self$missingness, episode_id, site)
    
    for (i in names(.self$conf)) {
        missconf <- .self$conf[[i]][["missingness_2d"]][["labels"]]
        if(!is.null(missconf)) {
            for (c in seq(missconf)) {
                col_name <- names(missconf[c])
                colr <- missconf[[c]]
                tbq <- selectTable(.self$record, items_opt=i, freq=colr)
                setkey(tbq, episode_id, site)
                oldnm <- names(.self$missingness)
                .self$missingness <- 
                    merge(.self$missingness, missingness_count(tbq))
                setnames(.self$missingness, c(oldnm, paste(i, col_name, sep=".")))
            }
        }
    }    
})




ccDataTable2$methods(
filter.null = function(items=c("episode_id", "site")) {
    "remove the entire episode when the episode_id or site is NULL"
    for (i in items)
        .self$tclean <- .self.tclean[i != "NULL"]
})



ccDataTable2$methods(
reload.conf = function(file) {
    "reload yaml configuration."
    .self$conf=yaml.load_file(file)
})
