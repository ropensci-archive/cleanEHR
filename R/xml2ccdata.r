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
getXmlepisode <- function(xml.root, id) {
    xml.root[[1]][[2]][[id]]
}

#' Extract the original file name from a path and file removing
#' all the suffixes.
#' @param pathfile a particular file name which may have a suffix
#' @param removestr last bit from the original filename
#' @return string
#' @export extract_file_origin
extract_file_origin <- function(pathfile, removestr='.xml'){
  split_path <- unlist(strsplit(pathfile, "/"))
  filename <- split_path[length(split_path)]
  original <- unlist(strsplit(filename, removestr))
  return(paste(original[1], removestr, sep=""))
  }


#' convert xml data to ccdata format
#' @param file xml file name
#' @return ccdata 
#' @export xml2Data
xml2Data <- function (file, select.episode=NULL, quiet=TRUE, xml=NULL,
                      file_origin="NA", parse_time=Sys.time()){
  if (is.null(xml)) {
    if (file_origin == "NA") {
      file_origin <- extract_file_origin(file)
    }
    xml <- xmlLoad(file)
  }

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
