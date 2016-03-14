#' @section Slots: 
#'   \describe{
#'      \item{\code{data}:}{list}
#'    }
#' @export cEpisode
cEpisode <- setClass("cEpisode",
                     slot=c(data="list"),
                     prototype=list(data=list()))


#' add single or multiple record to episode.
addItemData <- function(obj, data) {
    if (ncol(data) == 2) { # 1d data
        if (any(names(data) != c("id", "val"))) stop("1d data must have columns: id, val.")
        # assign row by row to episode data 
        for (i in seq(nrow(data))) {
            id <- as.character(data[i, "id"])
            if (is.null(obj@data[[id]]))
                obj@data[[id]] <- as.character(data[i,"val"])
            else
                stop("data already exist.")
        }
    }
    else if (ncol(data) == 3){# time-wise data
        if (any(names(data) != c("id", "time", "val")))
            stop("2d data must have columns: id, time, val.")
        # assign row by row to episode data
        for (i in seq(nrow(data))) {
            id <- as.character(data[i, "id"])
            if (is.null(obj@data[[id]]))
                obj@data[[id]] <- data[i, c("time", "val")]
            else
                obj@data[[id]] <- 
                    rbind(obj@data[[id]], data[i, c("time","val")])
        }
    }
    else stop("data must have either 2 or 3 columns.")
    return(obj)
}


#' add single or multiple record to episode.
setMethod('+', c("cEpisode", "data.frame"), 
          function(e1, e2) {addItemData(e1,e2)}
          ) 
