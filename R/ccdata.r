#' @include ccEpisode.r
###' @section Slots: 
##'   \describe{
##'      \item{\code{hospital.id}:}{vector}
##'      \item{\code{patient.id}:}{vector}
##'      \item{\code{var.names:}}{vector}
##'      \item{\code{var.id:}}{vector}
##'      \item{\code{patient.num:}}{integer}
##'      \item{\code{data.1d:}}{vector}
##'      \item{\code{data.2d:}}{vector, store timewise data}
##'    }
#' @exportClass ccRecord 
#' @export ccRecord
ccRecord <- setClass("ccRecord",
                     slots=c(npatient="integer",
                             nhs_numbers="data.table",
                             pas_numbers="data.table",
                             patients="list",
                             CLEANED_TAG="logical",
                             GOOD_INDEX="logical",
                             AGGREGATED_PATIENT_TAG="logical",
                             data_quality="list"),
                     prototype=prototype(npatient=as.integer(0),
                                         patients=list(),
                                         CLEANED_TAG=FALSE,
                                         GOOD_INDEX=FALSE,
                                         AGGREGATED_PATIENT_TAG=FALSE,
                                         data_quality=list()))


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


#' @export addDataListToRecord
addDataListToRecord <- function(rec, data) {
    if (length(data) == 0) #empty list
        return(rec)

    rec@CLEANED_TAG <- FALSE
    rec@patients <- 
        append(rec@patients, 
               lapply(data, 
                      function(p) {
                          env <- environment()
                          patient <- ccPatient()
                          lapply(p, 
                                 function(e) {
                                     if(!is.null(e))
                                         env$patient <- env$patient + ccEpisode(e)
                                 })
                          if (patient@nepisode > 0)
                              return(patient)
                          else 
                              return(NULL)
                      }))
    rec <- reindexRecord(rec)
    return(rec)
}

setMethod('+', c("ccRecord", "list"), 
          function(e1, e2) {addDataListToRecord(e1, e2)}
          )

setMethod('+', c("ccRecord", "NULL"), 
          function(e1, e2) return(e1))


#' Correct the index and meta data information for ccRecord.
#' It takes only the patient level indexing information. 
#' @export reindexRecord
reindexRecord <- function(record) {
    # remove NULL patients.
    record@patients <- record@patients[!sapply(record@patients, is.null)]

    nhs_numbers <- allPatientsInfo(record, "nhs_number")
    pas_numbers <- allPatientsInfo(record, "pas_number")
    record@npatient <- length(record@patients)

    record@nhs_numbers <- data.table(index=seq(record@npatient), nhs_numbers)
    record@pas_numbers <- data.table(index=seq(record@npatient), pas_numbers)
    record@GOOD_INDEX <- TRUE
    return(record)
}

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


#' @exportMethod [
setMethod("[", c("ccRecord", "ANY", "ANY"), 
          function(x, i, j, k=NULL) {
              if (is.null(k))
                  return(x@patients[[i]]@episodes[[j]])
              else
                  return(x@patients[[i]]@episodes[[j]]@data[[k]])
          })

setMethod("[", c("ccRecord", "ANY", "missing"), 
          function(x, i, j) {
              return(x@patients[[i]])
          })



#' @export setValue
setValue <- function(record, p, e, d, val){
    eval.parent(substitute(
                           record@patients[[p]]@episodes[[e]]@data[[d]] <- val))
}
