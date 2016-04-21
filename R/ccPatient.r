#' Patient class data
#' @examples 
#' p <- ccPatient() #initialise an empty patient.
#' @exportClass ccPatient
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
                                  episodes=list()))
#' add ccEpisode to ccPatient object
#' @param patient ccPatient
#' @param episode ccEpisode
#' @return ccPatient
#' @usage
#' p <- ccPatient()
#' e <- ccEpisode()
#' p_new <- p + e
#' @export addEpisodeToPatient
addEpisodeToPatient <- function(patient, episode) {
    # check and assign PAS number to patient.
    if (length(patient@pas_number) == 0)# empty patient 
        patient@pas_number <- episode@pas_number
    else { # patient has a non-empty pas_number
        if (patient@pas_number == 'NULL')                   
            patient@pas_number <- episode@pas_number
        else if (patient@pas_number != episode@pas_number & episode@pas_number != 'NULL')
            stop('ccPatient - Mismatching PAS number', 
                 patient@pas_number, "!=", episode@pas_number)
    }

    # check and assign NHS number to patient. 
    if (length(patient@nhs_number) == 0) # empty patient
        patient@nhs_number <- episode@nhs_number
    else { # patient has a non-empty nhs_number
        if (patient@nhs_number == 'NULL')
            patient@nhs_number <- episode@nhs_number
        else if (patient@nhs_number != episode@nhs_number & episode@nhs_number != 'NULL')
            stop('ccPatient - Mismatching NHS number', 
                 patient@nhs_number, "!=", episode@nhs_number)
    }

    # assigning data
    patient@episode_ids <- c(patient@episode_ids, episode@episode_id)
    patient@site_ids <- c(patient@site_ids, episode@site_id)

    patient@nepisode <- as.integer(patient@nepisode + 1)
    patient@episodes[[length(patient@episodes) + 1]] <- episode
    return(patient)
}


#' add a list of episode to a ccPatient object
#' @param patient ccPatient
#' @param eplist list contains ccPatient objects
#' @export addEpisodeListToPatient
addEpisodeListToPatient <- function(patient, eplist) {
    for (i in eplist)
        patient <- patient + i
    return(patient)
}

#' overload the addEpisode to patient.
#' @exportMethod +
setMethod('+', c("ccPatient", "ccEpisode"), 
          function(e1, e2) {addEpisodeToPatient(e1, e2)}
          )

#' @exportMethod +
setMethod('+', c("ccPatient", "list"), 
          function(e1, e2) {addEpisodeListToPatient(e1, e2)})
