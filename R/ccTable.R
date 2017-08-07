#' Rearrange and clean the critical care record into a 2D table.
#'
#' Data rearranging and major data cleaning processes will be performed under
#' the ccTable structre.  It holds the original record (ccRecord), the dirty table
#' (torigin) clean table (tclean) and various data quality information (dquality).
#' Various data filters can also be found within the ccTable class. 
#'
#' @field record ccRecord.
#' @field conf the YAML style configuration.
#' @field torigin the original wide data table.
#' @field tclean the wide data table after cleaning processes. 
#' @field dfilter list contains data filtering information.
#' @field dquality list contains data quality. 
#' @field summary list
#' @field base_cadence the base cadence is specified in hours
#' @include ccRecord.R
#' @examples
#' rec <- ccRecord()
#' cctable <- create.cctable(rec, freq=1)
#' cctable <- cctable$clean()
#' #table <- cctable$tclean 
#' @exportClass ccTable
ccTable <- setRefClass("ccTable", 
                            fields=c(
                                     record="ccRecord", 
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
    cat("$tclean", "\n")
    #print(.self$tclean)
    cat("Data entry (origin) = ", nrow(.self$torigin), "\n")
    uniepisode <- .self$torigin[,1,by=c("episode_id", "site")]
    cat("Episode number (origin) = ", nrow(uniepisode), "\n")
    cat("The base cadence is ", .self$base_cadence, " hour.\n")
})

#' Create the ccTable object from ccRecord
#'
#' @param rec ccRecord
#' @param conf either the path of YAML configuration file or the configuration
#'        structure in list.
#' @param freq the data cadence in hour.
#' @return ccTable object
#' @export
create.cctable <- function(rec, freq, conf=NULL) {
    if (is.null(conf)) 
        conf <- ITEM_REF
    else { 
        if (is.character(conf))
            conf <- yaml.load_file(conf)
    }

    cct <- ccTable(record=rec, conf=conf)
    cct$create.table(freq)
    return(cct)
}

#' Get the dfilter
#'
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



#' Create the ccTable object
#'
#' @name ccTable_create_cctable
#' @param freq numeric blah blah blah
#' @return numeric
NULL 
ccTable$methods(
    create.table = function(freq){
        "Create a table contains the selected items in the conf with a given
        frequency (in hour)"
        .self$items <- names(.self$conf)
        .self$torigin <- selectTable(record=record, items_opt=items, freq=freq)
        if (nrow(.self$torigin) != 0) {
            setkey(.self$torigin, "site", "episode_id")
            .self$tclean <- .self$torigin
            setkey(.self$torigin, "site", "episode_id")
            .self$base_cadence <- freq

            .self$.rindex <- .self$torigin
            for(i in .self$items) .self$.rindex[[i]] <- TRUE

            .self$.epindex <- .self$torigin[, TRUE, by=c("site", "episode_id")]
            setnames(.self$.epindex, c("site", "episode_id", "index"))
        } else 
            .self$torigin <- data.table(site=character(), 
                                        episode_id=character(),
                                        time=integer())
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
    apply.filters = function(warnings=T) {
        "Apply all filters specified in the configuration to update the clean
        table (tclean)"
       ops <- strsplit(grep('apply', names(unlist(.self$conf)), value=TRUE), "[.]") 
        for (i in ops) {
            item <- i[1]
            filter <- i[2]
             tryCatch(.self$spec2function(item, filter)(item,
                                                        .self$dfilter[[filter]]), 
                      error = function(e) {
                          if (is.null(.self$dfilter[[filter]])) {
                              if (warnings)
                                  warning(paste(item, "filter", filter, 
                                    "has been specified in the configuration but has not been ran."))
                          }
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
        "remove the entire episode when any of the selected items is NULL"
        for (i in items)
            .self$tclean <- .self.tclean[i != "NULL"]
})

ccTable$methods(
    reload.conf = function(file) {
        "reload yaml configuration."
        .self$conf <- yaml.load_file(file)
})


ccTable$methods(
    export.csv = function(file=NULL) {
        "Export the cleaned table to a CSV file."
        if (is.null(file))
            return(.self$tclean)

        write.csv(.self$tclean, file=file)
})


ccTable$methods(
    clean = function() {
        if (nrow(.self$torigin) != 0 ) {
            .self$filter.ranges()
            .self$filter.category()
            .self$filter.missingess()
            .self$filter.nodata()
            .self$apply.filters()
        }
        else 
            warning("The original table is NULL, hence no cleaning process has been performed.")
    })


#' Create wide table from ccRecord
#' 
#' @param record ccRecord
#' @param items_opt character vectors. Items (HIC code) selected in item_opt are optional items, which will be automatically 
#' filled when item is missing. 
#' @param items_obg obligatory items that is obligatory; Any episode that doesn't contain
#' item in this vector will be removed.
#' @param freq numeric cadence in hour. 
#' @param return_list logical if TRUE return as a list.  
#' @return data.table
#' @export selectTable
selectTable <- function(record, items_opt=NULL, items_obg=NULL, freq,
                        return_list=FALSE) {
    all_items <- c(items_opt, items_obg)
    if (is.null(all_items))
        stop('both items_opt and items_obg are NULL')

    env <- environment()
    lt <- list()
    stopifnot(is.list(env$lt)) # totally redundent, just to avoid an anonying
    # note says env is assigned but not used!
    for_each_episode(record,
                     function(ep) {
                         if (all(items_obg %in% names(ep@data))) {
                             result <- list()
                             period_length <- getEpisodePeriod(ep)
                             # getEpisodePeriod will return NULL when no 2D
                             # data been found. 
                             if (!is.null(period_length)) {
                                 if (period_length > 0 ) {
                                     result <- append(result,
                                                      itemsToDataFrame(ep, all_items,
                                                                       period_length,
                                                                       freq))
                                     nlength <- length(result[["time"]])
                                     result[["site"]] <- rep(ep@site_id, nlength)
                                     result[["episode_id"]] <- rep(ep@episode_id, nlength)
                                     env$lt[[length(lt) + 1]]<- .simple.data.frame(result)
                                 }
                             }
                         }
                     })
    if (return_list)
        return(lt)

    # fill is true because meta data column can be missing. 
    dt <- rbindlist(lt, fill=TRUE) 

   
    # Adding missing meta columns to keep the 2d wide consistent. 
    code.has.meta <- names(unlist(sapply(ITEM_REF, function(x) x$NHICmetaCode)))
    for (i in all_items) {
        meta.code <- paste(i, "meta", sep=".")
        if (i %in% code.has.meta & !(meta.code %in% names(dt))) {
            dt[[meta.code]] <- rep("NA", nrow(dt))
        }
    }

    # convert data type 
    for (i in all_items)
        dt[[i]] <- suppressWarnings(.which.datatype(i)(as.character(dt[[i]])))


    return(dt)
}

itemsToDataFrame <- function(ep, items, period_length, freq) {
    listmatrix <- list()
    time <- seq(0, period_length, freq)

    listmatrix[["time"]] <- time

    for (i in items) {
        if (length(ep@data[[i]]) == 1) {
            listmatrix[[i]] <- rep(ep@data[[i]], length(time))
        }
        else {
            if ("time" %in% names(ep@data[[i]])) {
                new <- reallocateTime(ep@data[[i]], period_length, freq)
                listmatrix[[i]] <- new$val
                if ("meta" %in% names(ep@data[[i]]))
                    listmatrix[[paste(i, "meta", sep=".")]] <- new$meta
            }
            else
                listmatrix[[i]] <- rep("NA", length(time))
        }
    }
    return(listmatrix)
}


#' @importFrom Rcpp evalCpp
#' @useDynLib cleanEHR 
reallocateTime <- function(d, t_discharge, frequency) {
    d_ <- d
    stopifnot(any(names(d) == "time"))
    stopifnot(any(names(d) == "item2d"))
    stopifnot(class(d$time) == "numeric")
    return(reallocateTime_(d_, t_discharge, frequency))
}


findMaxTime <- function(episode) {
    get2dTime <- function(episode){
        time_lt <- 
            lapply(episode@data, 
                   function(item){
                       if(length(item) > 1) {
                           if (!is.numeric(item$time))
                               item$time <-
                                   as.numeric(as.character(item$time))
                               return(max(item$time))
                       }
                   })
        tm <- unlist(time_lt)
        tm
    }
    tm <- get2dTime(episode)
    if (is.null(tm))
        return(NULL)
    else
        return(max(tm))
}


#' Get the length of stay based on the first and the last data point. 
#' 
#' @param e ccEpisode object.
#' @param unit character string.  Units in which the results are desired. Can be abbreviated.
#' @return length of stay
#' @export getEpisodePeriod
getEpisodePeriod <- function (e, unit="hours") {
    # pseudo delta period, see addPseudoTime()
    if (class(e@t_discharge)[1] == "numeric")
        return(e@t_discharge)

    if (class(e@t_admission)[1] != "POSIXct")
        tadm <- xmlTime2POSIX(as.character(e@t_admission), allow=T)
    else 
        tadm <- e@t_admission
    if (class(e@t_discharge)[1] != "POSIXct")
        tdisc <- xmlTime2POSIX(as.character(e@t_discharge), allow=T)
    else 
        tdisc <- e@t_discharge

    # The failure of POSIX conversion indicates that this episode is either 
    # anonymised or has a missing or incorrect value of discharge or admission
    # time. 
    if (is.na(tadm) || is.na(tdisc))
        period_length <- findMaxTime(e)
    else {
        if (any(is.null(tdisc), is.null(tadm)))
            period_length <- NULL
        else
            period_length <- as.numeric(tdisc - tadm,
                                        units=unit)
    }
    # in cases that tdisc == tadm
    if (!is.null(period_length)) {
        if (period_length == 0)
            period_length <- period_length + 1
    }

    if (is.null(period_length))
        warning("This episode does not have any time series data: ", 
                " episode_id = ", e@episode_id, 
                " nhs_number = ", e@nhs_number, 
                " pas_number = ", e@pas_number,
                " period_length = ", period_length, "\n")


    return(period_length)
}

#' Propagate a numerical delta time interval record.
#' @param record ccRecord
#' @param delta time frequency in hours
#' @details when discharge time and admission time are missing, the latest  and
#' the earliest data time stamp will be used instead.
#' @export reallocateTimeRecord
reallocateTimeRecord <- function(record, delta=0.5) {
    reallocate.episode <- function(e) {
        env <- environment()
        # make sure admin and disc time is correct
        period_length <- getEpisodePeriod(e)
        if (period_length < 0)  warning("period length < 0")
        
        # calling reallocateTime for each data item
        new.episode(lapply(e@data, 
               function(d) {
                   if (length(d) > 1) {
                       return(reallocateTime(d, env$period_length, delta))
                   } else 
                       return(d)
               }))
    }
    newdata <- for_each_episode(record, reallocate.episode)
    return(ccRecord() + newdata)
}
