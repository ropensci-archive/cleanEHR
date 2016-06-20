#' this is ref example
#' @import data.table
#' @include ccdata.r
#' @export ccDataTable
ccDataTable <- setRefClass("ccDataTable", 
                            fields=c(
                                     record="ccRecord", 
                                     conf="list",
                                     torigin="data.table", 
                                     tclean="data.table",
                                     missingness="data.table",
                                     range="data.table",
                                     summary="list",
                                     base_cadence="numeric"))
ccDataTable$methods(
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
#        .self$missingness.show()
#    }
#    else 
#        cat("no cleaning data can be found.\n")


})

#' @export count.present
count.present <- function(table, item) {
    return(table[,
          100 - (length(which(is.na(.SD[[item]]))) + 
              length(which(as.character(.SD[[item]]) == "NA")))/.N * 100,
          by=c("site", "episode_id")])
}

ccDataTable$methods(
    missingness.show = function()
        if(is.null(.self$missingness)) {
            cat("no missingness check available.\n")
        }
        else {
            txt <- vector()
            for(i in names(.self$conf)){
                itm <- .self$conf[[i]]
                acc <- itm[["missingness_2d"]][["accept_2d"]]
                threshold <- itm[["missingness_2d"]][['labels']][[names(acc)]]
                check_name <- paste(i, names(acc), sep='.')
                txt <- rbind(txt, 
                             c(NHIC=i, 
                               shortname=itm[["shortName"]],
                               "origin_data %"=
                                   round(mean(.self$missingness[[check_name]]), digits=2),
                               "clean_data  %"=
                                   round(mean(count.present(.self$tclean, i)$V1), digits=2),
                               "threshold %"=
                                   paste(acc[[1]], ' (', threshold, 'h - ', names(acc),  ')', sep="") 
                               )
                             ) 

            }
            pandoc.table(txt, style='simple')
        }
)

ccDataTable$methods(
    create.table = function(freq){
        "Create a table contains the selected items in the conf with a given
        frequency (in hour)"
        items <- names(.self$conf)
        .self$torigin <- selectTable(record=record, items_opt=items, 
                                     freq=freq)
        .self$tclean <- .self$torigin
        .self$base_cadence <- freq
})

ccDataTable$methods(
    filter.missingness = function(recount=FALSE){
        "filter out the where missingness is too low."
        if (recount || is.null(.self$missingness) ||
            nrow(.self$missingness) == 0)
            .self$get.missingness()

        if (is.null(.self$tclean) || nrow(.self$tclean) == 0)
            .self$tclean <- .self$torigin

        thresholds <- 
            unlist(lapply(.self$conf, 
                          function(x) x[["missingness_2d"]][["accept_2d"]]))

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


ccDataTable$methods(
    get.missingness = function() {
        miss_count <- function(tb_) { 
            cmplt <- function(vec) {
                length(which(vec!="NA"))/length(vec) * 100 
            }
            items <- names(tb_)[!names(tb_) %in% c("site", "time", "episode_id")]
            flags <- tb_[, cmplt(.SD[[items[1]]]), .(episode_id, site)]
            setnames(flags, c('episode_id', 'site', items[1]))
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
    filter.null = function(items=c("episode_id", "site")) {
        "remove the entire episode when the episode_id or site is NULL"
        for (i in items)
            .self$tclean <- .self.tclean[i != "NULL"]
})



ccDataTable$methods(
    reload.conf = function(file) {
        "reload yaml configuration."
        .self$conf=yaml.load_file(file)
})


ccDataTable$methods(
    imputation = function() {
        imputation_columns <- function(sd) {
            for (i in names(.self$conf)) {
                imwin <- .self$conf[[i]][['missingness_2d']][['impute_2d']]
                if (!is.null(imwin)) {
                    fun <- imwin[['fun']]
                    lead <- imwin[['lead']]
                    lag <- imwin[['lag']]
                    sd[[i]] <- interpolateVec(v=sd[[i]], lead=lead, lag=lag, FUN=fun, na.rm=T)
                }
            }
            return(sd)
        }
        .self$tclean <- .self$tclean[, imputation_columns(.SD), by=c("episode_id", "site")]
})


#' Check if the values of a vector v is in the ranges.
#' @param v vector numeric
#' @param range characters numeric ranges in a form such as (low, up). Multiple
#' ranges should seperated by semi-column.
inrange <- function(v, range) {
    funtxt <- function(r) {
        r <- gsub(";", "|", r)
        r <- gsub(",", " < v & v < ", r)
        return(paste("function(v)", r))
    }

    cmpfunc <- eval(parse(text=funtxt(range)))
    return(cmpfunc(v))
}


ccDataTable$methods(
    get.ranges = function(){
        if (is.null(.self$range))
            .self$range <- data.table(seq(nrow(.self$torigin)))#.self$torigin[,c('site', 'episode_id'), with=F]
        rgnum <- list('red'=1, 'amber'=2, 'green'=3)
        for(item_name in names(.self$conf)) {
            item <- .self$conf[[item_name]]

            if (!is.null(item[['range']])){
                rgclass <- rep(NA, nrow(.self$torigin))
                rgclass[.self$torigin[[item_name]] != "NA"] <- 0
                                
                for(rg_label in names(item[['range']])) {
                    irg <- item[['range']][[rg_label]]
                    rgclass[which(inrange(.self$torigin[[item_name]], irg))] <- 
                        rgnum[[rg_label]]
                }
                .self$range[[item_name]] <- rgclass
            }
        }
    }
)

ccDataTable$methods(
    filter.ranges = function(select='red') {
        rgnum <- list('red'=1, 'amber'=2, 'green'=3)
        if(is.null(.self$range) || nrow(.self$range) != nrow(.self$tclean))
            .self$get.ranges()
        for(item in names(.self$range)) 
            .self$tclean[[item]][.self$range[[item]] < rgnum[[select]]] <- NA 
    }
)
