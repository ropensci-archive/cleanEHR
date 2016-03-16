.getEpisodeData <- function(ep, items=NULL) {
    if (is.null(items))
        items <- names(ep@data)
    df_1d <- data.frame()
    df_2d <- data.frame()
    for (i in items) {
        data <- ep@data[[i]]
        if (class(data) == "character") {# 1d data
            df_1d <- 
                rbind(df_1d, 
                      data.frame(label=i, time="NA", val=data,
                                 stringsAsFactors=FALSE))
        }
        else if (class(data) == "data.frame") { # 2d data
            df_2d <-
                rbind(df_2d, data.frame(label=i, data,
                                        stringsAsFactors=FALSE))
        }
    }
    return(list(data1d=df_1d, data2d=df_2d))
}

# getEpisodeData
getEpisodeData <- function(ep, items=NULL) {
    if (is.null(items))
        items <- names(ep@data)
    data <- .getEpisodeData(ep, items)
    return(rbind(data$data1d, data$data2d))
}


#' get episode time table
getEpisodeTimeTable <- function(ep, items=NULL) {
    if (is.null(items))
        items <- names(ep@data)

    data <- .getEpisodeData(ep, items)
    df_1d <- data$data1d
    df_2d <- data$data2d

    if (nrow(df_2d) != 0) {
        labels <- sort(unique(items))
        time <- sort(unique(df_2d$time))
        index_label <- seq(labels)
        index_time <- seq(time)
        names(index_label) <- labels
        names(index_time) <- time

        time_table <- data.frame(array("", c(length(time), length(labels))),
                                 stringsAsFactors=FALSE)

        for (r in seq(nrow(df_2d))) {
            i <- index_time[as.character(df_2d$time[r])]
            j <- index_label[as.character(df_2d$label[r])]
            time_table[i, j] <- as.character(df_2d$val[r])
        }

        time_table <- cbind(time, time_table)
        colnames(time_table) <- c("time", labels)
    }
    else 
        time_table <- data.frame()

    return(list(data1d=df_1d, data2d=time_table))
}
