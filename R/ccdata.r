
ccdata.env <- new.env()
assign('code_pas_number',
       "NIHR_HIC_ICU_0001",
       #           getItemInfo("PAS number")["NHIC_code"],
       envir=ccdata.env)
assign('code_episode_id',
       "NIHR_HIC_ICU_0005",
       #       getItemInfo("Critical care local identifier / ICNARC admission number")["NHIC_code"],
       envir=ccdata.env)
#}


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
                     slots=c(hospital="character",
                             npatient="integer",
                             patient_id="character",
                             patients="list"),
                     prototype=c(hospital=character(0),
                                 npatient=as.integer(0),
                                 patient_id=integer(0),
                                 patients=list())
                     )

ccPatient <- setClass("ccPatient",
                      slot=c(patient_id="character",
                             episode_ids="character",
                             nepisode="integer",
                             episodes="list"),
                      prototype=c(patient_id="NA",
                                  episode_ids=character(0),
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
          c("ccPatient", "cEpisode"),
          function(obj, episode) {
              code_pas_number <- ccdata.env$code_pas_number
              code_episode_id <- ccdata.env$code_episode_id

              if (is.null(episode@data[[code_pas_number]]))
                  stop("no PAS number is found in the episode.")
              else
                  pas_number <- episode@data[[code_pas_number]]

              if (is.null(episode@data[[code_episode_id]]))
                  stop("no episode id is found in the episode.")
              else
                  episode_id <- episode@data[[code_episode_id]]
              if (obj@patient_id != pas_number & obj@patient_id != "NA")
                  stop("PAS number is not identical, you can only add the same patient here.")

              if (obj@patient_id == "NA")
                  obj@patient_id <- episode@data[[code_pas_number]]
              obj@episode_ids <- c(obj@episode_ids, episode_id)
              obj@nepisode <- as.integer(obj@nepisode + 1)
              obj@episodes[[episode_id]] <- episode
              return(obj)
          })

#' overload the addEpisode to patient. 
setMethod('+', c("ccPatient", "cEpisode"), 
          function(e1, e2) {addEpisode(e1, e2)}
          )

setMethod("addEpisode", 
          c("ccRecord", "cEpisode"),
          function(obj, episode) {
              code_pas_number <- ccdata.env$code_pas_number
              code_episode_id <- ccdata.env$code_episode_id

              if (is.null(episode@data[[code_pas_number]])) {
                  warning("no PAS number is found in the episode.")
                  return(obj)
              }
              else
                  pas_number <- episode@data[[code_pas_number]]

              if (is.null(episode@data[[code_episode_id]])) {
                  warning("no episode id is found in the episode.")
                  return(obj)
              }
              else
                  episode_id <- episode@data[[code_episode_id]]

              if (is.null(obj@patients[[pas_number]])) {
                  new.patient <- ccPatient()
                  new.patient <- new.patient + episode
                  obj@patients[[pas_number]] <- new.patient
                  obj@npatient <- obj@npatient + as.integer(1)
                  obj@patient_id <- c(obj@patient_id, pas_number)
              }
              else {
                  if (any(obj@patients[[pas_number]]@episode_ids ==
                          episode@data[[code_episode_id]]))
                      stop("found duplicated episode ids of an unique patient.")
                  obj@patients[[pas_number]] <-
                      obj@patients[[pas_number]] + episode
              }
              return(obj)
          })

#' overload the addEpisode to patient. 
setMethod('+', c("ccRecord", "cEpisode"), 
          function(e1, e2) {addEpisode(e1, e2)}
          )

