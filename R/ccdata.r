library(data.table)

#' @section Slots: 
#'   \describe{
#'      \item{\code{hospital.id}:}{vector}
#'      \item{\code{patient.id}:}{vector}
#'      \item{\code{var.names:}}{vector}
#'      \item{\code{var.id:}}{vector}
#'      \item{\code{patient.num:}}{integer}
#'      \item{\code{data.1d:}}{vector}
#'      \item{\code{data.2d:}}{vector, store timewise data}
#'    }
#' @export ccdata
ccRecord <- setClass("ccRecord",
                     slots=c(npatient="integer",
                             nhs_numbers="data.table",
                             pas_numbers="data.table",
                             CLEANED_TAG="logical",
                             patients="list"),
                     prototype=c(npatient=as.integer(0),
                                 patients=list(),
                                 CLEANED_TAG=FALSE,
                                 AGGREGATED_PATIENT_TAG=FALSE))

ccPatient <- setClass("ccPatient",
                      slot=c(pas_number="character",
                             nhs_number="character",
                             episode_ids="character",
                             site_ids="character",
                             nepisode="integer",
                             episodes="list"),
                      prototype=c(pas_number=character(0),
                                  nhs_number=character(0),
                                  episode_ids=character(0),
                                  site_ids=character(0),
                                  nepisode=as.integer(0),
                                  episodes=list())
                      )
#' addEpisode
setGeneric("addEpisode", 
           function(obj, episode) {
               standardGeneric("addEpisode")
           })

#' addEpisode
setMethod("addEpisode", 
          c("ccPatient", "ccEpisode"),
          function(obj, episode) {
              if (length(obj@pas_number) == 0) {
                  obj@pas_number <- episode@pas_number
              } else {
                  if (obj@pas_number != episode@pas_number) {
                      cat(obj@pas_number, "!=", episode@pas_number, "\n")
                      stop('ccPatient - Mismatching PAS number')
                  }
              }

              if (length(obj@nhs_number) == 0) {
                  obj@nhs_number <- episode@nhs_number
              } else {
                  if (obj@nhs_number != episode@nhs_number) {
                      cat(obj@nhs_number, "!=", episode@nhs_number, "\n")
                      stop('ccPatient - Mismatching NHS number')
                  }
              }

              obj@episode_ids <- c(obj@episode_ids, episode@episode_id)
              obj@site_ids <- c(obj@site_ids, episode@site_id)

              obj@nepisode <- as.integer(obj@nepisode + 1)
              obj@episodes[[length(obj@episodes) + 1]] <- episode
              return(obj)
          })


#' overload the addEpisode to patient. 
setMethod('+', c("ccPatient", "ccEpisode"), 
          function(e1, e2) {addEpisode(e1, e2)}
          )


setMethod("addEpisode", 
          c("ccRecord", "ccEpisode"),
          function(obj, episode) {
              new.patient <- ccPatient()
              new.patient <- new.patient + episode
              index <- length(obj@patients) + 1
              obj@patients[[index]] <- new.patient

              obj@nhs_numbers <- data.table(rbind(obj@nhs_numbers,
                                                  data.frame("index"=index, 
                                                       "nhs_number"=episode@nhs_number)))
              obj@pas_numbers <- data.table(rbind(obj@pas_numbers,
                                                  data.frame("index"=index,
                                                       "pas_number"=episode@pas_number)))
              obj@npatient <- as.integer(obj@npatient + 1)
              return(obj)
          })


#' overload the addEpisode to patient. 
setMethod('+', c("ccRecord", "ccEpisode"), 
          function(e1, e2) {addEpisode(e1, e2)}
          )
