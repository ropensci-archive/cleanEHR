#' Retrieve one item value at a time from all patients.
#' @param record is a ccRecord object.
#' @param item is a character which can be "nhs_number", "pas_number" and
#' "site_id"
#' @return a vector a all patients selected item. 
#' @usage
#' all.nhs.number <- allPatientsInfo(ccd, "nhs_number")
#' @export allPatientsInfo
allPatientsInfo <- function(record, item) {
    sapply(record@patients, function(x) {
                      ifelse(is.null(slot(x, item)), 
                             stop("slot ", item, 
                                  "cannot be found. Most probably caused by missing patient"), 
                             slot(x, item))
                      })
}


#' pick up the identical episode which has been injected by mistake.
#' @export duplicated.episode 
duplicated.episode <- function(cr) {
    nhs_number <- unlist(for_each_episode(cr, function(x) x@nhs_number))
    pas_number <- unlist(for_each_episode(cr, function(x) x@pas_number))
    site <- unlist(for_each_episode(cr, function(x) x@site_id))
    episode_id  <- unlist(for_each_episode(cr, function(x) 
                                          x@episode_id))

    adm <- unlist(for_each_episode(cr, function(x)
                                   x@data[[ccdata.env$code_admin_icu_t]]))
    disc <- unlist(for_each_episode(cr, function(x) 
                                   x@data[[ccdata.env$code_discharge_icu_t]]))

 # return(paste(site, episode_id, sep="_"))  
    return(paste(nhs_number, pas_number, site, episode_id, adm, disc, sep="_"))
}



#' getMissingRate
#' @export get_missing_rate 
get_missing_rate <- function(record, item, freq, sites=NULL, years=NULL) {
    datalist <- select_data(record, item=item, sites=sites, years=years, propgt=TRUE, 
                            freq=freq)
    missing_rate <- sapply(datalist, function(x)
                  length(which(x$val=="NA"))/length(x$val))
    missing_rate
}



#' Convert ccRecord data to item tables
#' @param record ccRecord
#' @param items if specified, items should be the NHIC code of the selected
#'              items, if not the output will contain be all the possible items, even
#'              those not appearing in the ccRecord
#' @param file  the name of csv files. If specified 1d/2d tables will be
#'              written into two separate csv files.
#' @export record2Table
record2Table <- function(record, items=NULL, file=NULL) {
    # the final output should have df_items either the selected items or all
    # possible items, to ensure the time_table for every episodes have a
    # unique number of columns. 
    # Leave the entire column empty when the item is not found.
    info <- extractInfo()
    if (is.null(items))  df_items <- c(info$time$id, info$nontime)
    else  df_items <- items

    df_1d <- data.frame()
    df_2d <- data.frame()
    patient_count <- 0
    for (pt in record@patients) {
        patient_count <- patient_count + 1
        for (ep in pt@episodes) {
            time_table <- getEpisodeItemTable(ep, df_items)
            patient_id <- patient_count
            episode_id <- ep@episode_id

            if (nrow(time_table$data2d) != 0)
                df_2d <- rbind(df_2d, data.frame(patient_id, episode_id,
                                                 time_table$data2d))
            if (nrow(time_table$data1d) != 0)
                df_1d <- rbind(df_1d, data.frame(patient_id, episode_id, 
                                                 time_table$data1d))
        }
    }
    if (nrow(df_2d) != 0) {
        item_code <- names(df_2d)[4:ncol(df_2d)]
        item_names <- as.vector(sapply(item_code, function(a) {getItemInfo(a)["item"]}))
        names(df_2d) <- c("patient_id", "episode_id", "time", item_names)
    }
    if (nrow(df_1d) != 0) {
        labels <- as.vector(sapply(df_1d$label, function(a) {getItemInfo(a)["item"]}))
        df_1d <- data.frame(patient_id=df_1d$patient_id,
                            episode_id=df_1d$episode_id,
                            label=labels,
                            val=df_1d$val)
        df_1d <- dcast(df_1d, patient_id + episode_id ~ label, value.var="val") # reshape the matrix
        if (nrow(df_2d) != 0) { # remove the 1d data columns of 2d data
            df_2d <- df_2d[, !names(df_2d) %in% names(df_1d[3:ncol(df_1d)])]
        }

    }

    if (!is.null(file)) {
        write.csv(file=paste(file, "_1d.csv", sep=""), df_1d)
        write.csv(file=paste(file, "_2d.csv", sep=""), df_2d)
    }
    invisible(list(data1d=df_1d, data2d=df_2d))
}


#' select one data item from ccRecord by unit by time
#' @export select_data
select_data <- function(record, item, sites=NULL, years=NULL, propgt=FALSE,
                        freq=NULL) 
{
    if (is.null(freq) & propgt) 
        stop("specify frequency if you want to propgt the variable")

    env <- environment()
    result <- list()

    for_each_episode(record, 
                     function(ep) {
                         # checking year and site id in the selecting list 
                         if (!is.null(years)) {
                             if (ep@episode_id == "NULL")
                                 epy <- "NULL"
                             else
                                 epy <- round(as.numeric(ep@episode_id)/10000)
                             if (epy %in% years) in_year <- TRUE
                             else in_year <- FALSE
                         } else 
                             in_year <- TRUE

                         if (!is.null(sites)) {
                             if (ep@site_id %in% sites) in_site <- TRUE
                             else in_site <- FALSE
                         } else 
                             in_site <- TRUE

                         # copy data out
                         if (in_site & in_year) {
                             if (propgt == TRUE) {
                                 if (length(ep@data[[item]]) > 1) {
                                     period_length <- getEpisodePeriod(ep)
                                     tryCatch(env$result[[length(env$result) + 1]] <- 
                                         reallocateTime(ep@data[[item]],
                                                        period_length,
                                                        freq),
                                              error=function(e) {
                                                  print(ep@data[[item]]); stop()
                                              })
                                 }
                             } else
                                 env$result[[length(env$result) + 1]] <- ep@data[[item]]
                         }

                     })

    return(result)
}


#' This is a simplified version of as.data.frame() with better performance. 
#' @export .simple.data.frame
.simple.data.frame <- function(x) {
    nm <- names(x)
    attr(x, "row.names") <- .set_row_names(length(x[[1]]))
    attr(x, "col.names") <- nm
    class(x) <- "data.frame"
    x
}



#' get indexing tables for time label, time-wise value, meta data label, and
#' meta data.  
#' @return list of vectors contains time.index, datat.index, meta.index,
#'          datam.index
extractIndexTable <- function() {
    info <- extractInfo()

    checklist <- list()
    for(i in seq(info$nontime))
        checklist[[info$nontime[i]]] <- "item1d"
    for(i in seq(info$time$idt))
        checklist[[info$time$idt[i]]] <-"time"
    for(i in seq(info$time$id))
        checklist[[info$time$id[i]]] <- "item2d"
    for(i in seq(info$meta$meta))
        checklist[[info$meta$meta[i]]] <- "meta"
    
    return(checklist)
}

#'
#' @export .which.type
.which.type <- function(id) {
    return(ccdata:::checklist[[id]])
}

#' 
#' @export .which.datatype
.which.datatype <- function(id) {
  # List with the conversion operations to do for each data type
  operations <- list('numeric' = as.numeric,
                     'text' = as.character,
                     'date' = as.character, # They are hashed for now
                     'time' = as.character, # They are hashed for now
                     'logical' = as.logical,
                     'list' = as.character,
                     'date/time' = as.character, # They are hashed for now
                     'list / logical' = as.character) # what are they?

  datatype = ccdata:::ITEM_REF[[id]]$Datatype
  if (!is.null(datatype)){
    if (exists(datatype, operations)){
      return(operations[[datatype]])
    }
  }
  # accounts for not listed or null (eg. when working with dt labels)
  return(as.character)
}


#' Convert item data to its corresponding data type.
#' @param id NHIC code of the data
#' @param vals The vector of values of the item.
#' @return vector values in its corresponding data type
which.datatype <- function(id, vals) {
}


#' give id number from NHIC code like "NIHR_HIC_ICU_xxxx"
#' @param nhic NHIC code
whichIsCode <- function(nhic) {
    return(grepl(nhic, pattern="[0-9][0-9][0-9][0-9]"))
}

#' extract information from data.checklist
#' @return list of time [data.frame(id, idt)], meta [data.frame(id, idmeta)], 
#'         nontime [numeric], MAX_NUM_NHIC
#' @export extractInfo
extractInfo <- function() {
    if(!exists("data.checklist"))
        data("data.checklist")
    index.time <- whichIsCode(data.checklist$NHICdtCode) 
    index.meta <- whichIsCode(data.checklist$NHICmetaCode)

    item.labels <- StdId(data.checklist$NHICcode[index.time])
    time.labels <- StdId(data.checklist$NHICdtCode[index.time])

    metaitem.labels <- StdId(data.checklist$NHICcode[index.meta])
    meta.labels <- StdId(data.checklist$NHICmetaCode[index.meta])
    
    time.list <-
        data.frame(id=item.labels@ids, idt=time.labels@ids,
                   stringsAsFactors=FALSE)
    meta.list <- data.frame(id=metaitem.labels@ids, meta=meta.labels@ids,
                            stringsAsFactors=FALSE)
    
    
    nontime<- StdId(data.checklist$NHICcode[!index.time])
    # get all ids which should be the assemble of NHICcode and NHICmetaCode
    all.nhic.code <- StdId(data.checklist$NHICcode)
    all.ids <- c(meta.list$idmeta,
                 all.nhic.code@ids)
    if (any(duplicated(all.ids)))
        stop("data.checklist.RData error! meta data code and NHICcode are overlaped")
    return(list(time=time.list, meta=meta.list, nontime=nontime@ids,
                MAX_NUM_NHIC=max(as.numeric(as.number(all.nhic.code)), 
                                 as.numeric(as.number(StdId(time.list$idt))))))
}

#' retrieve information of the query code/item names from data.checklist
#' @param item.code it can be either item name or NHIC_code, dt_code, or
#'        meta_code
#' @return a vector contains NHIC_code, dt_code, meta_code and row_in_checklist
#' @examples 
#' getItemInfo("Time of death on your unit")
#' getItemInfo("NIHR_HIC_ICU_0001")
#' @export getItemInfo
getItemInfo <- function(item.code) {
    if (!exists("data.checklist"))
        data("data.checklist")

    if(grepl("NIHR_HIC_ICU_", item.code)){# input is code
        item <- data.checklist$NHICcode == item.code
        dt <- data.checklist$NHICdtCode == item.code
        meta <- data.checklist$NHICmetaCode == item.code
        row.in.list <- which(item | dt | meta)
    }
    else{ # input is item name
        row.in.list <- which(data.checklist$dataItem==item.code)
    }

    if (length(row.in.list) != 1){
        stop("item/NHIC code cannot be found in the list.\n")
    }

    item.info <- c(as.character(data.checklist$dataItem[row.in.list]),
                   as.character(data.checklist$NHICcode[row.in.list]),
                   as.character(data.checklist$NHICdtCode[row.in.list]),
                   as.character(data.checklist$NHICmetaCode[row.in.list]),
                   as.character(data.checklist$Units[row.in.list]),
                   as.character(row.in.list))

    names(item.info) <- c("item", "NHIC_code", "dt_code", 
                          "meta_code", "unit", "row_in_checklist")
    return(item.info)
}

#' get information of a group of code of items and return an array.
getItemsInfo <- function(items.code, var) {
    info_ <- array(NA, length(items.code))
    for (i in seq(items.code))
        info_[i] <- getItemInfo(items.code[i])[var]
    return(info_)
}

#' convert time from xml table to POSIX format.
#' @param allow If allow is FASLE, it returns error when time format is wrong,
#' however it allows exceptions when "allow" is set to be true, where it
#' returns NA. It is useful when dealing with anonymised data.
#' e.g. 2014-02-01T03:00:00 -> 2014-02-01 03:00:00
#' @export xmlTime2POSIX
xmlTime2POSIX <- function(xml.time, allow=FALSE){
    if (is.null(xml.time))
        return(NA)
    tp <- as.POSIXct(xml.time, "GMT", format="%Y-%m-%dT%H:%M:%S")
    tp[is.na(tp)] <- as.POSIXct(xml.time[is.na(tp)], "GMT", format="%Y-%m-%d")
    if (!allow) {
        if(any(is.na(tp)))
            stop(xml.time[is.na(tp)])
    }
    return(tp)
}
