library("XML")

#' load xml clinical data
#' @return the root of the xml data
#' @export xmlLoad
xmlLoad <- function(file) {
    file.parse <- xmlParse(file)
    xml.root <- xmlRoot(file.parse)
    return(xml.root)
}

#' get the episode data from xml 
#' @param xml.root root of xml data returned by xmlLoad()
#' @return episode xml data
getXmlepisode <- function(xml.root, id) {
    xml.root[[1]][[2]][[id]]
}



#' convert xml data to ccdata format
#' @param xml can be either xml root or xml file name
#' @return ccdata 
#' @export xml2Data
xml2Data <- function (xml, select.episode=NULL, quiet=TRUE){
    if (typeof(xml) != "externalptr")
        xml <- xmlLoad(xml)
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
