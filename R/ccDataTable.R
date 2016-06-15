#' @exportClass ccQuality
ccQuality <- setClass("ccQuality", 
                      slots=c(data_range="list",
                              miss_2d="data.table"))

#' @export ccDataTable
ccDataTable <- setClass("ccDataTable", 
                        slots=c(table="data.table",
                                conf_yaml="list",
                                quality="ccQuality", 
                                base_frequency="character"))

#' This function reads first a yaml configuration file which cointains the
#' chosen items and their data quality information. It loads the selected items
#' and create a 2d missing rate table if the option check_miss2d is TRUE. 
#' @param record is a ccRecord object of which the time of 2d data should be 
#'        numerical delta time.
#' @param yaml is a character type string specifying the location of the yaml
#'        configuration file.
#' @param base_frequency integer frequency of the final table (in hour).
#' @export new.ccDataTable
new.ccDataTable <- function(record, yaml, base_frequency=1, check_miss2d=TRUE) {
    conf <- yaml.load_file(yaml)
    items <- names(conf)

    tb <- selectTable(record=record, items_opt=items, freq=base_frequency)
    qlty <- tb[, 1, by=c("episode_id", "site")] # how to specify a name here
    # instead of using V1?
    qlty[, V1:=NULL]
    setkey(qlty, episode_id, site)

    if (check_miss2d) {
        for (i in items) {
            missconf <- conf[[i]][["missingness_2d"]][["labels"]]
            if(!is.null(missconf)) {
                for (c in seq(missconf)) {
                    col_name <- names(missconf[c])
                    colr <- missconf[[c]]
                    tbq <- selectTable(record, items_opt=i, freq=colr)
                    setkey(tbq, episode_id, site)
                    oldnm <- names(qlty)
                    qlty <- merge(qlty, missingness_count(tbq))
                    setnames(qlty, c(oldnm, paste(i, col_name, sep=".")))
                }
            }
        }    
    }

    ccDataTable(table=tb, 
                quality=ccQuality(miss_2d=qlty), 
                conf_yaml=conf, 
                base_frequency=paste(base_frequency, "hour"))
}

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

#' @export remove_null
remove_null <- function(cdt) {
    cdt@table <- cdt@table[episode_id != "NULL" & site != "NULL"]
    cdt@quality@miss_2d <- cdt@quality@miss_2d[episode_id != "NULL" & site != "NULL"]
    cdt
}

#' @export filter_threshold_percentage
filter_threshold_percentage <- function(cdt, conf=NULL) {
    if (!is.null(conf)) 
        cdt@conf_yaml <- yaml.load_file(conf)

    thresholds <- 
        unlist(lapply(cdt@conf_yaml, 
                      function(x) x[["missingness_2d"]][["miss_acceptance"]]))
    
    # how to make functions data.table awared? so that we can avoid these ugly
    # indexing.
    select_index <- rep(TRUE, nrow(cdt@quality@miss_2d))
    for (nt in names(thresholds))
        select_index <- select_index & as.vector(cdt@quality@miss_2d[, nt, with=FALSE] >
                                        thresholds[nt])
    select_table <- cdt@quality@miss_2d[select_index]
    merge(select_table, cdt@table, by=c("episode_id", "site"))
}

#' @export imputation_all
imputation_all <- function(cdt, conf=NULL) {
    if (!is.null(conf)) 
        cdt@conf_yaml <- yaml.load_file(conf)


    imputation_columns <- function(sd) {
        for (i in names(cdt@conf_yaml)) {
            imwin <- cdt@conf_yaml[[i]][['missingness_2d']][['imputation_window']]
            lead <- imwin[['lead']]
            lag <- imwin[['lag']]
    
#        sd[[i]] <- interpolateVec(sd[[i]],
#                                  ,100, mean, na.rm=T)
#        sd
        }

    }
     b[, imputation_columns(.SD), by=c("episode_id", "site")]
    


}

