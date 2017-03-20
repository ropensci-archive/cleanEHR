#' Calculate the length of stay in the ICU. 
#'
#' Calculate the length of stay in the ICU and append it to the original demographic
#' table. 
#' @param demg data.table the demograhic table which should at least contain
#' column DAICU and DDICU
#' @param units character The unit of lenstay column, by default the output will be in hours 
#' @return data.table It is the original data.table with lenstay column (in 
#' difftime) appended. 
#' @export lenstay
lenstay <- function(demg, units="hours") {
    len <- difftime(xmlTime2POSIX(demg$DDICU, allow=T), 
             xmlTime2POSIX(demg$DAICU, allow=T),
             units=units)
    return(cbind(demg, lenstay = len))
}
