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
#' @export ccRecord
ccRecord <- setClass("ccRecord",
                     slots=c(npatient="integer",
                             nhs_numbers="data.table",
                             pas_numbers="data.table",
                             patients="list",
                             CLEANED_TAG="logical",
                             AGGREGATED_PATIENT_TAG="logical"),
                     prototype=prototype(npatient=as.integer(0),
                                 patients=list(),
                                 CLEANED_TAG=FALSE,
                                 AGGREGATED_PATIENT_TAG=FALSE))
#' Patient class data
#' @examples 
#' p <- ccPatient() #initialise an empty patient.
#' @export ccPatient
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
              # check and assign PAS number to patient.
              if (length(obj@pas_number) == 0)# empty patient 
                  obj@pas_number <- episode@pas_number
              else { # patient has a non-empty pas_number
                  if (obj@pas_number == 'NULL')                   
                      obj@pas_number <- episode@pas_number
                  else if (obj@pas_number != episode@pas_number & episode@pas_number != 'NULL')
                      stop('ccPatient - Mismatching PAS number', 
                           obj@pas_number, "!=", episode@pas_number)
              }

              # check and assign NHS number to patient. 
              if (length(obj@nhs_number) == 0) # empty patient
                  obj@nhs_number <- episode@nhs_number
              else { # patient has a non-empty nhs_number
                  if (obj@nhs_number == 'NULL')
                      obj@nhs_number <- episode@nhs_number
                  else if (obj@nhs_number != episode@nhs_number & episode@nhs_number != 'NULL')
                      stop('ccPatient - Mismatching NHS number', 
                           obj@nhs_number, "!=", episode@nhs_number)
              }

              # assigning data
              obj@episode_ids <- c(obj@episode_ids, episode@episode_id)
              obj@site_ids <- c(obj@site_ids, episode@site_id)

              obj@nepisode <- as.integer(obj@nepisode + 1)
              obj@episodes[[length(obj@episodes) + 1]] <- episode
              return(obj)
          })


#' overload the addEpisode to patient.
#' @exportMethod +
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


setGeneric("aggPatients",
           function(record) {
               standardGeneric("aggPatients")
           })


addRecord <- function(rec1, rec2) {
    rec1@npatient <- rec1@npatient + rec2@npatient
    rec1@patients <- append(rec1@patients, rec2@patients)

    nhs_number <-sapply(rec1@patients, function(x) x@nhs_number)
    pas_number <-sapply(rec1@patients, function(x) x@pas_number)
    
    rec1@nhs_numbers <- data.table(index=seq(nhs_number), nhs_number=nhs_number)
    rec1@pas_numbers <- data.table(index=seq(pas_number), pas_number=pas_number)
    return(rec1)
}

setMethod('+', c("ccRecord", "ccRecord"), 
          function(e1, e2) {addRecord(e1, e2)}
          )


#' Aggregate episodes with the same patient ids (e.g. nhs_number or pas_number)
#' @param record ccRecord
setMethod("aggPatients", 
          c("ccRecord"),
          function(record) {
              nhs.duplicated.index<-
                  record@nhs_number[duplicated(ccd@nhs_number[!'NULL'])]$index
              pas.duplicated.index <-
                  record@pas_number[duplicated(ccd@pas_number[!'NULL'])]$index

              duplicated.index <- sort(unique(as.integer(c(nhs.duplicated.index,
                                                    pas.duplicated.index))))
              
              for(id in duplicated.index) {
                  for (ep in record@patients[[id]]@episodes) {
                      patient_index <- patientIndex(record,
                                                    record@patient[[id]])

                      record@patients[[id]]
                  }
                  a<-record@patients[[id]]@episodes[[1]]
              }
          })
