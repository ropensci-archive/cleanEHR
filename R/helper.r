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
