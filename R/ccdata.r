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
                   slots=c(hospital.id="vector",
                           patient.id="vector",
                           var.names="vector",
                           var.id="vector",
                           patient.num="integer",
                           data.1d="list",
                           data.2d="list"),
                   validity=function(object) {
                       if (length(object@var.id) != length(object@var.names))
                           return("The length of var.id should be equal to the lenght of var names.")
                       else
                           return(TRUE)
                   })

