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
ccData <- setClass("ccdata",
                   slots=c(episode.num="integer",
                           episodes="list"),
                   validity=function(object) {
                       if (length(object@var.id) != length(object@var.names))
                           return("The length of var.id should be equal to the lenght of var names.")
                       else
                           return(TRUE)
                   })

cEpisode <- setClass("cEpisode",
                     slot=c(
                            data="list"
                            ),
                     prototype=list(
                                    data=list()
                                    )
                     )


setGeneric(name="addItemData",
           def=function(obj, data)
           {
               standardGeneric("addItemData")
           })

#' as 1D data 

#' as 2D data 
setMethod(f="addItemData", 
          signature=signature(obj="cEpisode", 
                              data="data.frame"
                              ),
          definition=function(obj, data) {
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
          })





