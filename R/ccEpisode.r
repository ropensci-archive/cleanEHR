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
                             data="list"),
                      prototype=list(data=list(),
                                     episode_id="NULL",
                                     pas_number="NULL",
                                     nhs_number="NULL",
                                     site_id="NULL"))

#' add data in a data frame to an episode.
#' @param obj episode
#' @param data data frame with either 2 columns (id, val) or 3 columns
addItemData <- function(obj, data) {
    if (ncol(data) == 2) { # 1d data
        if (any(names(data) != c("id", "val"))) { 
            stop("1d data must have columns: id, val.")
        }

        # assign row by row to episode data 
        for (i in seq(nrow(data))) {
            id <- as.character(data[i, "id"])
            if (is.null(obj@data[[id]])) {
                obj@data[[id]] <- as.character(data[i,"val"])
                if (id == ccdata.env$code_nhs_number)
                    obj@nhs_number <- obj@data[[id]]
                if (id == ccdata.env$code_pas_number)
                    obj@pas_number <- obj@data[[id]]
                if (id == ccdata.env$code_episode_id)
                    obj@episode_id <- obj@data[[id]]
                if (id == ccdata.env$code_site_id)
                    obj@site_id <- obj@data[[id]]
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
ccEpisode <- function(data=NULL, ...) {
    if (is.null(data))
        return(new("ccEpisode", ...))

    new.ep<- new("ccEpisode", data=data)
    if (!is.null(data[[ccdata.env$code_nhs_number]]))
        new.ep@nhs_number <- data[[ccdata.env$code_nhs_number]]
    if (!is.null(data[[ccdata.env$code_pas_number]]))
        new.ep@pas_number <- data[[ccdata.env$code_pas_number]]
    if (!is.null(data[[ccdata.env$code_episode_id]]))
        new.ep@episode_id <- data[[ccdata.env$code_episode_id]]
    if (!is.null(data[[ccdata.env$code_site_id]]))
        new.ep@site_id <- data[[ccdata.env$code_site_id]]
    return(new.ep)
}

#' add single or multiple record to episode.
#' @exportMethod +
setMethod('+', c("ccEpisode", "data.frame"), 
          function(e1, e2) {addItemData(e1,e2)}
          ) 
