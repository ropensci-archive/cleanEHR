#' @exportClass ccQuality
ccQuality <- setClass("ccQuality", 
                      slots=c(data_range="list",
                              miss_2d="data.table"))

#' @export ccDataTable
ccDataTable <- setClass("ccDataTable", 
                        slots=c(table="data.table", 
                                quality="ccQuality"))


#' @export new.ccDataTable
new.ccDataTable <- function(record, yaml, base_frequency=1) {
    conf <- yaml.load_file(yaml)
    items <- names(conf)

    tb <- selectTable(record=record, items_opt=items, freq=base_frequency)
    qlty <- tb[, 1, by=c("episode_id", "site")] # how to specify a name here
                                                # instead of using V1?
    qlty[, V1:=NULL]
    setkey(qlty, episode_id, site)

    for (i in items) {
        missconf <- conf[[i]][["missingness_2d"]]
        if(!is.null(conf[[i]][["missingness_2d"]])) {
            for (c in seq(missconf)) {
                col_name <- names(missconf[c])
                colr <- missconf[[c]]
                tbq <- selectTable(record, items_opt=items, freq=colr)
                setkey(tbq, episode_id, site)
                oldnm <- names(qlty)
                qlty <- merge(qlty, missingness_count(tbq))
                setnames(qlty, c(oldnm, paste(i, col_name, sep=".")))
            }
        }
    }    
    
    cdt <- ccDataTable(table=tb, quality=ccQuality(miss_2d=qlty))
    return(cdt)
}

#' @export missingness_count
missingness_count <- function(tb) {
    cmplt <- function(vec) {
        length(which(vec!="NA"))/length(vec) * 100
    }
    items <- names(tb)[!names(tb) %in% c("site", "time", "episode_id")]
    stopifnot(length(items)==1)
    flags <- tb[, cmplt(.SD[[items[1]]]), .(episode_id, site)]
    setnames(flags, c('episode_id', 'site', items))
    
    flags
}


