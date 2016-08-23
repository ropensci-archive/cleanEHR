#' @section Slots: 
#'   \describe{
#'      \item{\code{episode_id}: {character}}
#'      \item{\code{pas_number}: {character}}
#'      \item{\code{nhs_number}: {character}}
#'      \item{\code{site_id}: {character}}
#'      \item{\code{data}:}{list}
#'    }
#' @exportClass ccEpisode
#' @export ccEpisode
ccEpisode <- setClass("ccEpisode",
                      slot=c(episode_id="character",
                             pas_number="character",
                             nhs_number="character",
                             site_id="character",
                             admin_icu_time="ANY",
                             discharge_icu_time="ANY",
                             live_dead="character",
                             file_origin="character",
                             parse_time="POSIXct",
                             data="list"),
                      prototype=list(data=list(),
                                     episode_id="NULL",
                                     pas_number="NULL",
                                     nhs_number="NULL",
                                     site_id="NULL",
                                     admin_icu_time="NULL",
                                     discharge_icu_time="NULL",
                                     live_dead="NULL",
                                     file_origin="NULL",
                                     parse_time=as.POSIXct(NA)))

#' add data in a data frame to an episode.
#' @param obj episode
#' @param data data frame with either 2 columns (id, val) or 3 columns
addItemData <- function(obj, data) {
    if (ncol(data) == 2) { # 1d data
        if (any(names(data) != c("id", "val"))) { 
            stop("1d data must have columns: id, val.")
        }
        nhscode <- stname2code("NHSNO")
        pascode <- stname2code("pasno")
        epidcode <- stname2code("ADNO")
        sitecode <- stname2code("ICNNO")


        # assign row by row to episode data 
        for (i in seq(nrow(data))) {
            id <- as.character(data[i, "id"])
            if (is.null(obj@data[[id]])) {
                obj@data[[id]] <- as.character(data[i,"val"])
                if (id == nhscode)  obj@nhs_number <- obj@data[[id]]
                if (id == pascode)  obj@pas_number <- obj@data[[id]]
                if (id == epidcode) obj@episode_id <- obj@data[[id]]
                if (id == sitecode) obj@site_id <- obj@data[[id]]
            }
            else {
                stop("data already exist.")
            }
        }
    } else if (ncol(data) == 3) {# time-wise data
        if (any(names(data) != c("id", "time", "val"))) {
            stop("2d data must have columns: id, time, val.")
        }

        # assign row by row to episode data
        for (i in seq(nrow(data))) {
            id <- as.character(data[i, "id"])
            if (is.null(obj@data[[id]])) {
                obj@data[[id]] <- data[i, c("time", "val")]
            }
            else {
                obj@data[[id]] <- 
                    rbind(obj@data[[id]], data[i, c("time","val")])
            }
        }
    }
    else stop("data must have either 2 or 3 columns.")
    return(obj)
}


#' ccEpisode
#' @export ccEpisode
ccEpisode <- function(data=NULL, anon=FALSE, ...) {
    if (is.null(data))
        return(new("ccEpisode", ...))

    nhscode <- stname2code("NHSNO")
    pascode <- stname2code("pasno")
    epidcode <- stname2code("ADNO")
    sitecode <- stname2code("ICNNO")
    ldcode <- stname2code("DIS")
    admtcode <- stname2code("DAICU")
    disccode <- stname2code("DDICU")

    new.ep <- new("ccEpisode", data=data)
    if (!is.null(data[[nhscode]]))
        new.ep@nhs_number <- data[[nhscode]]

    if (!is.null(data[[pascode]]))
        new.ep@pas_number <- data[[pascode]]

    if (!is.null(data[[epidcode]]))
        new.ep@episode_id <- data[[epidcode]]

    if (!is.null(data[[sitecode]]))
        new.ep@site_id <- data[[sitecode]]

    if (!is.null(data[[disccode]]))
        new.ep@discharge_icu_time <-
            xmlTime2POSIX(data[[disccode]], allow=T)

    if (!is.null(data[[admtcode]]))
        new.ep@admin_icu_time <-
            xmlTime2POSIX(data[[admtcode]], allow=T)

    if (!is.null(data[[ldcode]]))
        new.ep@live_dead <- data[[ldcode]]

    return(new.ep)
}

#' add single or multiple record to episode.
#' @exportMethod +
setMethod('+', c("ccEpisode", "data.frame"), 
          function(e1, e2) {addItemData(e1,e2)}
          ) 
