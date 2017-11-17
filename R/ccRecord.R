#' @import XML
#' @import data.table
#' @import yaml
#' @importFrom Rcpp evalCpp
NULL


#' The S4 class which holds all the CCHIC patient record - served as a database.
#'
#' @description  ccRecord is a class to hold the raw episode data parsed directly 
#' from XML or CSV files.
#' @field nepisodes is an integer number indicates the total number of episode
#'       the record is holding.
#' @field dmgtb a data.table containing all the demographic information of each
#'       episode, including site_id, NHS number, PAS number, admission date/time,
#'       and discharge date/time. This field is usually left empty.
#' @field infotb a data.table holding the parsing information of each episode such as the
#'       parsing time and from which file it parsed from.
#' @field episdoes a list of ccEpisode objects. 
#' @exportClass ccRecord
#' @export ccRecord
#' @examples
#' heart_rate <- data.frame(seq(10), rep(70, 10)) # NIHR_HIC_ICU_0108
#' site_id <- "Q70" #  NIHR_HIC_ICU_0002
#' episode_id <- "0000001" # NIHR_HIC_ICU_0005
#'
#' # Create a new episode 
#' ep <- new.episode(list(NIHR_HIC_ICU_0108=heart_rate, 
#'                          NIHR_HIC_ICU_0002=site_id, 
#'                          NIHR_HIC_ICU_0005=episode_id)) 
#' 
#' # modifying records 
#' rec <- ccRecord() # a new record 
#' rec <- rec + ep # adding a new episode to the record
#' rec <- rec + NULL # adding nothing to the record
#' rec <- rec + rec # adding a record to a record
#' # Adding a list of episodes 
#' rec <- ccRecord()
#' ep1 <- new.episode()
#' ep2 <- new.episode()
#' eps.list <- list(ep1, ep2)
#' new.rec <- rec + eps.list
ccRecord <- setClass("ccRecord", 
                      slots=c(nepisodes="integer", dmgtb="data.table", 
                              infotb="data.table", episodes="list"),
                      prototype=prototype(nepisodes=as.integer(0), 
                                          infotb=data.table(), 
                                          dmgtb=data.table()))

#' The S4 class which holds data of a single episode. 
#' 
#' @field site_id character string. Site ID, if presented, otherwise "NA".
#' @field episode_id character string. Episode ID, if presented, otherwise "NA".
#' @field nhs_number character string. NHS number, if presented, otherwise "NA".
#' @field pas_number character string. PAS number, if presented, otherwise "NA".
#' @field parse_file character string. The source XML file. If the source is not a file then "NA".
#' @field t_admission POSIXct. Time of Admission to the ICU, if presented, otherwise NA.
#' @field t_discharge POSIXct. Time of discharge of the ICU, if presented, otherwise NA.
#' @field parse_time POSIXct. Parse time. 
#' @field data A list which holds all the data of this episode which is indexed by NIHIC code. 
#' @exportClass ccEpisode 
ccEpisode <- setClass("ccEpisode", 
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
    rec@infotb <- rbindlist(for_each_episode(rec, retrieve_all))

    # id will be filled in the following sequence, NHS number, PAS number,
    # site-episode combination and unknown tags. 
    if (nrow(rec@infotb) > 1) {
        id <- rec@infotb$nhs_number
        id[id=="NA"] <- rec@infotb$pas_number[id=="NA"]
        id[id=="NA"] <- paste(rec@infotb$site_id[id=="NA"], 
                                rec@infotb$episode_id[id=="NA"],
                                sep="-")
        id[id=="NA-NA"] <- paste("unknown", seq(length(which(id=="NA-NA"))))
        id <- data.table(id=id)
        id[, "pid":=.GRP, by="id"]
        rec@infotb[, "pid":=id$pid]
        rec@infotb[, "index":=seq(nrow(rec@infotb))]
    }
    rec
}

#' Adding a list of ccEpisode to ccRecord
#' 
#' @description Adding a list of one or multiple ccEpisode objects to a
#' ccRecord object, the information table (infotb) will be updated automatically.
#' It is the more efficient way to add multiple ccEpisode objects.
#' @param e1 ccRecord
#' @param e2 a list of ccEpisode objects
#' @return ccRecord
#' @exportMethod +
setMethod('+', c("ccRecord", "list"), 
          function(e1, e2) {
              for(i in seq(length(e2)))
                  e1@episodes[[length(e1@episodes) + 1]] <- e2[[i]]
              index.record(e1)
         
          
          })

#' Adding one ccEpisode object to a ccRecord 
#' 
#' @param e1 ccRecord-class
#' @param e2 ccEpisode-class
#' @return ccRecord-class 
setMethod('+', c("ccRecord", "ccEpisode"), 
          function(e1, e2) {
              e1@episodes[[length(e1@episodes) + 1]] <- e2
              index.record(e1)
          })

#' Combine two ccRecord objects 
#' 
#' @param e1 ccRecord-class
#' @param e2 ccRecord-class
#' @return ccRecord-class
setMethod('+', c("ccRecord", "ccRecord"), 
          function(e1, e2) {
              e1@episodes <- append(e1@episodes, e2@episodes)
              index.record(e1)
          })

#' Adding nothing to a ccRecord object and return the original ccRecord
#' 
#' @param e1 ccRecord-class 
#' @param e2 NULL 
setMethod('+', c("ccRecord", "NULL"), 
          function(e1, e2) return(e1))


#' Create a new episode
#' 
#' create a new ccEpisode object by given the episode data as a
#' list. The list should be organised in data items and indexed with NIHC code,
#' e.g. NIHR_HIC_ICU_0108. 
#'
#' @param lt is a list
#' @param parse_file the file location from which the episode comes from.
#' @param parse_time the parse date and time of the episode.
#' @return ccEpisode object
#' @examples 
#' eps <- list()
#' eps[["NIHR_HIC_ICU_0018"]] <- data.frame(time=seq(10), rep(70, 10))
#' new.episode(eps)
#' 
#' @export 
new.episode <- function(lt=list(), 
                        parse_file="NA", 
                        parse_time=as.POSIXct(NA)) { 
    eps <- ccEpisode()
    eps@data <- lt
    
    short.name <- c("NHSNO", "pasno", "ADNO", "ICNNO")
    slot.name  <- c("nhs_number", "pas_number", "episode_id", "site_id")
    
    # character values 
    for (i in seq(slot.name)) {
        val <- lt[[stname2code(short.name[i])]]
        if (is.null(val)) slot(eps, slot.name[i]) <- "NA"
        else slot(eps, slot.name[i]) <- val
    } # Time data 
    short.name <- c("DAICU", "DDICU")
    slot.name <- c("t_admission", "t_discharge")
    for (i in seq(slot.name)) 
        slot(eps, slot.name[i]) <-
            as.POSIXct(xmlTime2POSIX(lt[[stname2code(short.name[i])]], allow=TRUE))

    eps@parse_file <- parse_file
    eps@parse_time <- parse_time 
    eps
}

#' loop over all episodes of a ccRecord object 
#' 
#' @param record ccRecord 
#' @param fun function 
#' @export 
for_each_episode <- function(record, fun) {
    lapply(record@episodes, fun)
}


#' Subsetting a ccRecord object and return a list of ccEpisode objects.
#' 
#' @param x ccRecord-class
#' @param i integer vector
#' @exportMethod [[
setMethod("[[", "ccRecord",
          function(x, i) {
              eplst <- list()
              for (ep in i) {
                  eplst[[length(eplst) + 1]] <- x@episodes[[ep]]
              }
              eplst
          }
)

#' Create a subset of ccRecord object from the original one via specifying the row number of episodes.
#'
#' @param x ccRecord-class
#' @param i integer vector
#' @exportMethod [
setMethod("[", "ccRecord",
          function(x, i){ 
              eplst <- list()
              for (ep in i) {
                  eplst[[length(eplst) + 1]] <- x@episodes[[ep]]
              }
              ccRecord() + eplst
          })

#' Create a ccRecord subsetting via selected sites.
#'
#' @param x ccRecord-class
#' @param i character vector which contains site_ids, e.g. c("Q70", "Q70W")
#' @exportMethod [
setMethod("[", signature(x="ccRecord", i="character"), 
          definition=function(x, i) {
              stopifnot(all(i%in%rownames(site.info())))
              ind <- x@infotb[x@infotb$site_id%in%i]$index
              if (length(ind) == 0) {
                  return(ccRecord())
              }
              eplst <- list()
              for (ep in ind) {
                  eplst[[length(eplst) + 1]] <- x@episodes[[ep]]
              }
              ccRecord() + eplst
          })

#' Get a subset of episodes from ccRecord. 
#' 
#' @param r ccRecord-class 
#' @param f character a vector of XML file names - see ccRecord: parse_file 
#' @return ccRecord-class
#' @exportMethod subset
setGeneric("subset", function(r, f) {
    standardGeneric("subset")
})


#' Get a subset of episodes that have the same from ccRecord . 
#' 
#' @param r ccRecord-class
#' @param f character a vector of XML file names - see ccRecord: parse_file 
#' @return ccRecord object 
setMethod("subset", signature(r="ccRecord", f="character"), 
function(r, f) {
    ind <- r@infotb[r@infotb$parse_file %in% f]$index
    if (length(ind) == 0) {
        return(ccRecord())
    }
    eplst <- list()
    for (ep in ind) {
        eplst[[length(eplst) + 1]] <- r@episodes[[ep]]
    }
    ccRecord() + eplst
})

episode_graph <- function(ep, items=NULL) {
    t_ad <- ep@t_admission
    t_dc <- ep@t_discharge


    if (is.null(items))
        items <- c("h_rate", "spo2", "bilirubin", "platelets", "pao2_fio2", "gcs_total")

    all.drugs <- names(which(class.dict_code[names(ITEM_REF)] == "Drugs"))
    used.drugs <- code2stname(all.drugs[all.drugs %in% names(ep@data)])

    classification.dictionary <- sapply(ITEM_REF, function(x) x$Classification1)


    create.long.table <- function(ep, items) {
        items <- data.table(items=items, 
                            code=stname2code(items),
                            longname=stname2longname(items),
                            class=classification.dictionary[stname2code(items)])
        units <- unit.dict[items$code]
        units[is.na(units)] <- ""
        items$longname <- paste0(items$longname, "\n", units)

        ltb <- list()
        for (i in seq(nrow(items))) {
            if (is.null(ep@data[[items[i]$code]]))
                ltb[[i]] <- data.frame()
            else
                ltb[[i]] <- data.frame(ep@data[[items[i]$code]], 
                                       item=items[i]$longname)
        }
        ltb <- rbindlist(ltb, use.names=TRUE, fill=TRUE)
        if (is.numeric(ltb$time))
            ltb$time <- t_ad + ltb$time * 60 * 60
        ltb$item2d <- as.numeric(ltb$item2d)
        return(ltb)
    }

    physio.tb <- create.long.table(ep, items)
    physio.tb <- data.frame(physio.tb, 
                            catg1=physio.tb$item, 
                            catg2="Physiology Data")
    drug.tb <- create.long.table(ep, used.drugs)

    drug.tb <- data.frame(drug.tb, catg1="Drugs", 
                          catg2=drug.tb$item)


    tb <- rbindlist(list(physio.tb, drug.tb), fill=TRUE, use.names=TRUE)


    ggp <- ggplot(tb, aes_string(x="time", y="item2d", group="item",
                                 colour="catg2")) + geom_line(colour="#1E506C") + 
        geom_point(size=1) + 
        facet_grid(catg1 ~., scales="free_y") + 
        geom_vline(xintercept = as.numeric(t_ad), colour="#D1746F") + 
        geom_vline(xintercept = as.numeric(t_dc), colour="#D1746F") + 
        scale_colour_manual(values=c("#1E506C", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FFFFFF"), 
                            name=paste0(ep@episode_id, "_", ep@site_id, "\n", 
                                       icnarc2diagnosis(ep@data[[stname2code('RAICU1')]]), "\n\n")) +  
        theme(legend.title = element_text(size=8), 
              legend.text  = element_text(size=8)) +
                            labs(x="", y="")



    graphics::plot(ggp)
    #"#1E506C""#D1746F"
    invisible(tb)
}

#' Individual episode chart
#' 
#' Create an individual episode chart for its diagnosis, drugs and physiological
#' variables. Diagnosis and drugs are always included, while the user can
#' select other longitudinal data. 
#' @param r ccEpisode-class
#' @param v short name of longitudinal data. While v is not given, the chart 
#' will only display h_rate, spo2, bilirubin, platelets, pao2_fio2, gcs_total. 
#' @return a table of selected vars of an episode
#' @exportMethod plot
#' @examples
#' \dontrun{
#' plot(ccd@episodes[[1]]) # plot first episode with default variables. 
#' plot(ccd@episodes[[1]], "h_rate") # plot first episode heart rate
#' }
setGeneric("plot", function(r, v) {
    standardGeneric("plot")
})

#' Episode chart
#' 
#' @param r ccEpisode-class
#' @param v character 
setMethod("plot", signature(r="ccEpisode", v="character"), 
function(r, v){
    episode_graph(r, v)
})


#' Episode chart default fields
#' 
#' @param r ccEpisode-class
setMethod("plot", signature(r="ccEpisode", v="missing"), 
function(r) {
    episode_graph(r)
})
