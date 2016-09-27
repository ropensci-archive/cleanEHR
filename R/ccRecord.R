#' @include ccEpisode.R
NULL

#' @title A class to hold raw episode data.
#' @description  ccRecord is a class to hold the raw episode data parsed directly from XML or
#' CSV files.
#' @slot nepisodes is an integer number indicates the total number of episode
#'       the record is holding.
#' @slot dmgc a data.table containing all the demographic information of each
#'       episode, including site_id, NHS number, PAS number, admission date/time,
#'       and discharge date/time. 
#' @slot parseinfo a data.table holding the parsing information of each episode such as the
#'       parsing time and from which file it parsed from.
#' @exportClass ccRecord2
ccRecord2 <- setClass("ccRecord2", 
                      slots=c(nepisodes="integer", dmgc="data.table", 
                              parseinfo="data.table", episodes="list"),
                      prototype=prototype(nepisodes=as.integer(0)))

#' adding episode data as a list to ccRecord
#' @param rec ccRecord2
#' @param 
add.episode.to.record <- function(rec, episode) {
}


ccEpisode2 <- setClass("ccEpisode2", 
                       slots=c(site_id="character", 
                               episode_id="character",
                               nhs_number="character",
                               pas_number="character",
                               t_admission="POSIXct", 
                               t_discharge="POSIXct", 
                               data="list"), 

                       prototype=prototype(site_id="NA", 
                                           episode_id="NA", 
                                           nhs_number="NA",
                                           pas_number="NA", 
                                           t_admission=as.POSIXct(NA), 
                                           t_discharge=as.POSIXct(NA), 
                                           data=list()))

#' @title Create a new episode
#' @description create a new ccEpisode2 object by given the episode data as a
#' list. The list should be organised in data items. See examples. 
#' @param lt is a list  
#' @return ccEpisode object
#' @examples 
#' eps <- list()
#' eps[["NIHR_HIC_ICU_0018"]] <- data.frame(time=seq(10), rep(70, 10))
#' new.episode(eps)
#' @export new.episode 
new.episode <- function(lt) {
    eps <- ccEpisode2()
    eps@data <- lt
    
    short.name <- c("NHSNO", "pasno", "ADNO", "ICNNO")
    slot.name  <- c("nhs_number", "pas_number", "episode_id", "site_id")
    
    # character values 
    for (i in seq(slot.name)) {
        val <- lt[[stname2code(short.name[i])]]
        if (is.null(val)) slot(eps, slot.name[i]) <- "NA"
        else slot(eps, slot.name[i]) <- val
    }

    # Time data 
    short.name <- c("DAICU", "DDICU")
    slot.name <- c("t_admission", "t_discharge")
    for (i in seq(slot.name)) 
        slot(eps, slot.name[i]) <-
            as.POSIXct(xmlTime2POSIX(lt[[stname2code(short.name[i])]], allow=T))
    eps
}

