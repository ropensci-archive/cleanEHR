#' @include ccEpisode.R
NULL

#' @title A class to hold parsed episode data.
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
#' @export ccRecord2
#' @examples
#' heart_rate <- data.frame(seq(10), rep(70, 10)) # NIHR_HIC_ICU_0108
#' site_id <- "Q70" #  NIHR_HIC_ICU_0002
#' episode_id <- 0000001 # NIHR_HIC_ICU_0005
#'
#' # Create a new episode 
#' ep <- new.ccEpisode2(list(NIHR_HIC_ICU_0108=heart_rate, NIHR_HIC_ICU_0002=site_id, NIHR_HIC_ICU_0005=episode_id)) 
#' 
#' # modifying records 
#' rec <- ccRecord2() # a new record 
#' rec <- rec + ep # adding a new episode to the record
#' rec <- rec + NULL # adding nothing to the record
#' rec <- rec + rec # adding a record to a record
ccRecord2 <- setClass("ccRecord2", 
                      slots=c(nepisodes="integer", dmgtb="data.table", 
                              infotb="data.table", episodes="list"),
                      prototype=prototype(nepisodes=as.integer(0)))

#' @title Adding one ccEpisode object to ccRecord2 object.
#' @param rec ccRecord2
#' @param episode ccEpisode2 object
#' @return ccRecord2 object
#' @export add.episode.to.record
add.episode.to.record <- function(rec, episode) {
    rec@episodes[[length(rec@episodes) + 1]] <- episode
    index.record(rec)
}

#' @title Adding a list of ccEpisode to ccRecord
#' @description Adding a list of one or multiple ccEpisode2 objects to
#' ccRecord2 object, the information table (infotb) will be updated automatically.
#' It is a more efficient way to add multiple ccEpisode2 objects. See
#' add.episode.to.record() for just adding one ccEpisode. 
#' @param rec ccRecord2
#' @param lst a list of ccEpisode2 objects
#' @return ccRecord2
#' @export add.episode.list.to.record
add.episode.list.to.record <- function(rec, lst) {
    for(i in seq(length(lst)))
        rec@episodes[[length(rec@episodes) + 1]] <- lst[[i]]
    index.record(rec)
}

#' @title Concatenating two ccRecords objects
#' @param rec1 ccRecord2 object
#' @param rec2 ccRecord2 object
#' @return ccRecord2 object
add.record.to.record <- function(rec1, rec2) {
    rec1@episodes <- append(rec1@episodes, rec2@episodes)
    index.record(rec1)
}

setMethod('+', c("ccRecord2", "list"), 
          function(e1, e2) {add.episode.list.to.record(e1, e2)}
          )

setMethod('+', c("ccRecord2", "ccEpisode"), 
          function(e1, e2) {add.episode.to.record(e1, e2)})


setMethod('+', c("ccRecord2", "ccRecord2"), 
          function(e1, e2) {add.record.to.record(e1, e2)}
          )

setMethod('+', c("ccRecord2", "NULL"), 
          function(e1, e2) return(e1))



#' @export index.record
index.record <- function(rec) {
    retrieve_all <- function(x) {
        .simple.data.frame(list(site_id    = x@site_id, 
                                episode_id = x@episode_id,
                                nhs_number = x@nhs_number, 
                                pas_number = x@pas_number, 
                                t_admission= x@t_admission, 
                                t_discharge= x@t_discharge, 
                                parse_file = x@parse_file, 
                                parse_time = x@parse_time))
    
    }
    rec@nepisodes <- length(rec@episodes)
    rec@infotb <- rbindlist(for_each_episode2(rec, retrieve_all))

    rec
}


# NOTE: put some description here.
ccEpisode2 <- setClass("ccEpisode2", 
                       slots=c(site_id="character", 
                               episode_id="character",
                               nhs_number="character",
                               pas_number="character",
                               t_admission="POSIXct", 
                               t_discharge="POSIXct",
                               parse_file="character",
                               parse_time="POSIXct", 
                               data="list"), 

                       prototype=prototype(site_id="NA", 
                                           episode_id="NA", 
                                           nhs_number="NA",
                                           pas_number="NA", 
                                           t_admission=as.POSIXct(NA), 
                                           t_discharge=as.POSIXct(NA), 
                                           parse_file="NA",
                                           parse_time=as.POSIXct(NA),
                                           data=list()))
#' @title Create a new episode
#' @description create a new ccEpisode2 object by given the episode data as a
#' list. The list should be organised in data items and indexed with NIHC code,
#' e.g. NIHR_HIC_ICU_0108. 
#' @param lt is a list
#' @param parse_file the file location from which the episode comes from.
#' @param parse_time the parse date and time of the episode.
#' @return ccEpisode object
#' @examples 
#' eps <- list()
#' eps[["NIHR_HIC_ICU_0018"]] <- data.frame(time=seq(10), rep(70, 10))
#' new.episode(eps)
#' @export new.episode 
new.episode <- function(lt, parse_file="NA", parse_time=as.POSIXct(NA)) {
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

    eps@parse_file <- parse_file
    eps@parse_time <- parse_time 
    eps
}


for_each_episode2 <- function(record, fun) {
    lapply(record@episodes, fun)
}
