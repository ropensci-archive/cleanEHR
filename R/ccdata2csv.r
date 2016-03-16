#' ccdata2csv
ccdata2csv <- function(ccdata, items=NULL) {
    df <- data.frame()
    for (pt in ccdata@patients) {
        for (ep in pt@episodes) {
            df <- rbind(df, getEpisodeTimeTable(ep, items))
#            df <- getEpisodeTimeTable(ep, items)
        }
    }
    return(df)
}
