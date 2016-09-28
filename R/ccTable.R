#' Rearrange 1d and 2d items into a wide table to enable data cleaning and
#' reporting process.
#' @field record ccRecord
#' @field dfilter list contains three data quality tables 1) range, 2)
#' missingness, 3) categorical
#' @import data.table
#' @include ccRecord.R
#' @exportClass ccTable
ccTable <- setRefClass("ccTable", 
                            fields=c(
                                     record="ccRecord2", 
                                     conf="list",
                                     torigin="data.table", 
                                     tclean="data.table",
                                     dfilter="list",
                                     dquality="list",
                                     summary="list",
                                     base_cadence="numeric",
                                     .rindex="data.table", 
                                     .epindex="data.table",
                                     items="character"))
ccTable$methods(
show = function() {
    panderOptions("table.split.table", 150)
    
    cat("$tclean", "\n")
    print(.self$tclean)
    cat("Data entry (origin) = ", nrow(.self$torigin), "\n")
    uniepisode <- .self$torigin[,1,by=c("episode_id", "site")]
    cat("Episode number (origin) = ", nrow(uniepisode), "\n")


#    if (!is.null(.self$tclean) & nrow(.self$tclean) != 0) {
#        uniepisode <- .self$tclean[,1,by=c("episode_id", "site")]
#        cat("Data entry (clean) = ", nrow(.self$tclean), "\n")
#        uniepisode <- .self$tclean[,1,by=c("episode_id", "site")]
#        cat("Episode number (clean) = ", nrow(uniepisode), "\n")
#        .self$dfilter[['#missingness']].show()
#    }
#    else 
#        cat("no cleaning# data can be found.\n")


})

#' construct function of ccTable object
#' @param rec ccRecord
#' @param conf either the path of YAML configuration file or the configuration
#'        structure in list.
#' @param freq the data cadence in hour.
#' @return ccTable object
#' @export create.cctable
create.cctable <- function(rec, freq, conf=NULL) {
    if (is.null(conf)) 
        conf <- ccdata:::ITEM_REF
    else { 
        if (is.character(conf))
            conf <- yaml.load_file(conf)
    }

    cct <- ccTable(record=rec, conf=conf)
    cct$create.table(freq)
    return(cct)
}

#' get the dfilter
#' @param dq can be either dqaulity table or torigin
#' @param criterion should be a function to give T/F values of each entry.
#' @export getfilter
getfilter <- function(dq, criterion) {
    if (ncol(dq) > 2) {
        keys <- dq[, c("site", "episode_id"), with=FALSE]
        dq[, c("site", "episode_id"):=NULL]
        # updating range entry with true/false values
        dq <- dq[, Map(criterion, .SD, names(.SD))]
        # adding site and episode_id columns.
        entry <- data.table(keys, dq)
        episode <- entry[, 
                         all(unlist(.SD), na.rm=TRUE), 
                         by=c("site", "episode_id")]
        setnames(episode, c("site", "episode_id", "select_index"))
        return(list(entry=entry, episode=episode))
    }
    else return(NULL)
}


ccTable$methods(
    create.table = function(freq){
        "Create a table contains the selected items in the conf with a given
        frequency (in hour)"
        .self$items <- names(.self$conf)
        .self$torigin <- selectTable(record=record, items_opt=items, freq=freq)
        setkey(.self$torigin, "site", "episode_id")
        .self$tclean <- .self$torigin
        setkey(.self$torigin, "site", "episode_id")
        .self$base_cadence <- freq

        .self$.rindex <- .self$torigin
        for(i in .self$items) .self$.rindex[[i]] <- TRUE

        .self$.epindex <- .self$torigin[, TRUE, by=c("site", "episode_id")]
        setnames(.self$.epindex, c("site", "episode_id", "index"))
})


ccTable$methods(
    update.entry = function(){
        for (i in .self$items) 
            .self$tclean[[i]][!.self$.rindex[[i]]] <- NA
    })


ccTable$methods(
    update.episode = function(){
        sep <- .self$.epindex[index==TRUE]
        .self$tclean <- merge(.self$tclean, sep, by=c("site", "episode_id"))
        .self$tclean[["index"]] <- NULL
    })

ccTable$methods(
    apply.filters = function() {
       ops <- strsplit(grep('apply', names(unlist(.self$conf)), value=TRUE), "[.]") 
        for (i in ops) {
            item <- i[1]
            filter <- i[2]
             tryCatch(.self$spec2function(item, filter)(item,
                                                        .self$dfilter[[filter]]), 
                      error = function(e) {
                          if (is.null(.self$dfilter[[filter]]))
                              warning(paste(item, "filter", filter, 
"has been specified in the configuration but has not been ran."))
                          else {
                              cat(paste(item, filter, "\n"))
                              stop(e)
                          }
                      }
            )
        }
        .self$update.entry()
        .self$update.episode()
    })

ccTable$methods(
    drop_entry = function(nmitem, dq){
        .self$.rindex[[nmitem]] <- 
            .self$.rindex[[nmitem]] & dq$entry[[nmitem]]
    })

ccTable$methods(
    drop_episode = function(nmitem, dq){
        .self$.epindex[["index"]] <- 
            .self$.epindex[["index"]] & dq$episode[["select_index"]]
    })

ccTable$methods(
    spec2function = function(item.name, filter.name) {
        spec <- .self$conf[[item.name]][[filter.name]]$apply
        spec <- as.character(as.vector(spec))
        switch(spec, 
               "drop_entry"=.self$drop_entry,
               "drop_episode"=.self$drop_episode,
               "NA"=function(nmitem, dq){}, 
               "NULL"=function(nmitem, dq){},
               stop("functions for applying filters can only be 'drop_entry' or 'drop_episode'. "))
})


ccTable$methods(
    filter.null = function(items=c("episode_id", "site")) {
        "remove the entire episode when the episode_id or site is NULL"
        for (i in items)
            .self$tclean <- .self.tclean[i != "NULL"]
})

ccTable$methods(
    reload.conf = function(file) {
        "reload yaml configuration."
        .self$conf=yaml.load_file(file)
})


ccTable$methods(
    export.csv = function(file) {
        "Export the cleaned table to a CSV file."
        write.csv(.self$tclean, file=file)
})
