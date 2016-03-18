#' Convert ccRecord data to item tables
#' @param record ccRecord
#' @param items if specified, items should be the NHIC code of the selected
#'              items, if not the output will contain be all the possible items, even
#'              those not appearing in the ccRecord
#' @param pseudo (optional) logical value, FALSE gives identifier the PAS number, TRUE gives
#'               the patient count in the record.
#' @param file  the name of csv files. If speicified 1d/2d tables will be
#'              written into two sperate csv files.
ccRecord2Table <- function(record, items=NULL, pseudo=FALSE, file=NULL) {
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
        episode_count <- 0

        for (epid in pt@episode_ids) {
            time_table <- getEpisodeItemTable(pt@episodes[[epid]], df_items)
            if (pseudo) {
                episode_count  <- episode_count + 1
                patient_id <- patient_count
                episode_id <- episode_count
            }
            else {
                patient_id <- pt@patient_id
                episode_id <- epid
            }
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
