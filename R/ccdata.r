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
#' @exportClass ccRecord 
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

#' add ccEpisode object to ccRecord object
#' @param recd ccRecord
#' @param episode ccEpisode
#' @return ccRecord
#' @export addEpisodeToRecord
#' @usage
#' r_new <- r + e
addEpisodeToRecord <- function(recd, episode) {
    new.patient <- ccPatient()
    new.patient <- new.patient + episode
    index <- length(recd@patients) + 1
    recd@patients[[index]] <- new.patient

    recd@nhs_numbers <- data.table(rbind(recd@nhs_numbers,
                                        data.frame("index"=index, 
                                                   "nhs_number"=episode@nhs_number)))
    recd@pas_numbers <- data.table(rbind(recd@pas_numbers,
                                        data.frame("index"=index,
                                                   "pas_number"=episode@pas_number)))
    recd@npatient <- as.integer(recd@npatient + 1)
    return(recd)
}

#' @exportMethod +
setMethod('+', c("ccRecord", "ccEpisode"), 
          function(e1, e2) {addEpisodeToRecord(e1, e2)}
          )



#' append two ccRecord objects 
#' @param rec1 ccRecord
#' @param rec2 ccRecord
#' @return ccRecord
#' @usage 
#' r_new <- r1 + r2
addRecord <- function(rec1, rec2) {
    rec1@npatient <- rec1@npatient + rec2@npatient
    rec1@patients <- append(rec1@patients, rec2@patients)

    nhs_number <-sapply(rec1@patients, function(x) x@nhs_number)
    pas_number <-sapply(rec1@patients, function(x) x@pas_number)

    rec1@nhs_numbers <- data.table(index=seq(nhs_number), nhs_number=nhs_number)
    rec1@pas_numbers <- data.table(index=seq(pas_number), pas_number=pas_number)
    return(rec1)
}

#' @exportMethod +
setMethod('+', c("ccRecord", "ccRecord"), 
          function(e1, e2) {addRecord(e1, e2)}
          )
