library("XML")

#' load xml clinical data
#' @return the root of the xml data
xmlLoad <- function(file) {
    file.parse <- xmlParse(file)
    xml.root <- xmlRoot(file.parse)
    return(xml.root)
}

#' get the episode data from xml 
#' @param xml.root root of xml data returned by xmlLoad()
getXmlepisode <- function(xml.root, id) {
    xml.root[[1]][[2]][[id]]
}

#' convert xml data to ccdata format
#' @param file xml file name
#' @return ccdata 
#' @export xml2Data
xml2Data <- function (file, select.episode=NULL, quiet=TRUE){
    
    parse_time <- Sys.time()
    split_file_name <- unlist(strsplit(file, "/"))
    file_origin <- split_file_name[length(split_file_name)]

    xml <- xmlLoad(file)
    
    episode.num <- xmlSize(xml[[1]][[2]])
    if(is.null(select.episode))
        select.episode <- seq(episode.num)


    if (!quiet)
        pb <- txtProgressBar(min = min(select.episode)-1, 
                             max = max(select.episode), style = 3)
    record <- ccRecord()

    for(episode.id in select.episode){
        episode <- getXmlepisode(xml, episode.id)
        episode_list <- tryCatch(xmlEpisodeToList(episode), 
                         error=function(err) {
                             cat(paste(err, "episode.id = ", episode.id, "\n"))
                             stop()
                         })
        episode_i <- ccEpisode(episode_list)
        episode_i@file_origin <- file_origin
        episode_i@parse_time <- parse_time
        record <- record + episode_i
        
        if (!quiet)
            setTxtProgressBar(pb, episode.id)
    }
    if (!quiet)
        cat("\n")

    setkey(record@nhs_numbers, nhs_number)
    setkey(record@pas_numbers, pas_number)

    return(record)
}
