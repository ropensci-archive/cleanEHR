#' ccdata2csv
ccdata2csv <- function(ccdata, items=NULL, pseudo=FALSE) {
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
    for (pt in ccdata@patients) {
        patient_count <- patient_count + 1
        episode_count <- 0

        for (epid in pt@episode_ids) {
            time_table <- getEpisodeTimeTable(pt@episodes[[epid]], items)
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
    }
    df_1d <- dcast(df_1d, patient_id + episode_id ~ label)

    return(list(data1d=df_1d, data2d=df_2d))
}

