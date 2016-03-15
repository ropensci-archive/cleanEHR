#' ccdata2csv
ccdata2csv <- function(ccdata) {


}

episode2dataframe <- function(ep, items=NULL) {
    if (is.null(items))
        items <- names(ep@data)
    df_dmgph <- data.frame()
    df_physiology <- data.frame()
    for (i in items) {
        data <- ep@data[[i]]
        if (class(data) == "character") {# 1d dmgphgraphic data
            df_dmgph <- 
                rbind(df_dmgph, 
                      data.frame(label=i, time="NA", val=data,
                                 stringsAsFactors=FALSE))
        }
        else if (class(data) == "data.frame") {
            df_physiology <-
                rbind(df_physiology, data.frame(label=i, data,
                                                   stringsAsFactors=FALSE))
        }
        else
            stop("unrecognised data type from episode.")
    }

    labels <- sort(unique(df_physiology$label))
    time <- sort(unique(df_physiology$time))
    index_label <- data.table(data.frame(labels, index=seq(labels)))
    index_time <- data.table(data.frame(time, index=seq(time)))
    setkey(index_label, labels)
    setkey(index_time, time)

    
    time_table <- data.frame(array("", c(length(time), length(labels))),
                             stringsAsFactors=FALSE)
    for (r in seq(nrow(df_physiology))) {
        i <- index_time[as.character(df_physiology$time[r]), index]
        print(df_physiology$time[r])
        print(index_time[i])
        j <- index_label[as.character(df_physiology$label[r]), index]
        time_table[i, j] <- as.character(df_physiology$val[r])
    }

    time_table <- cbind(time, time_table)
    colnames(time_table) <- c("time", labels)
    
    return(time_table)
}
