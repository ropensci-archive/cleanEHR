#' Create the demographic tables. The data type of each column is in its
#' corresponding data type.
#'
#' @param record ccRecord-class
#' @param dtype logical column will be type aware, else all in character. 
#' @export demographic.table
demographic.table <- function(record, dtype=TRUE) {
    env <- environment()
    demogls <- list()
    stopifnot(is.list(env$demogls))
    all.demcode <- all.nhic.code("Demographic")
    for_each_episode(record, 
        function(x){
            demog.data <- rep("NULL", length(all.demcode))
            names(demog.data) <- all.demcode
            demog.data <- as.list(demog.data)
            for(item in names(x@data)) {
                if (length(x@data[[item]]) == 1) {
                    demog.data[[item]] <- x@data[[item]]
                }
            }
            env$demogls[[length(env$demogls) + 1]] <- .simple.data.frame(demog.data)
        })
    demogt <- rbindlist(demogls, fill=TRUE)
    setnames(demogt, code2stname(names(demogt)))

    if (dtype) {
        for (i in seq(ncol(demogt))){
            demogt[[i]] <- 
                .which.datatype(stname2code(names(demogt)[i]))(demogt[[i]])
        }
    }
    demogt[, "index":=seq(nrow(demogt))]
    return(demogt)
}


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
    len <- difftime(xmlTime2POSIX(demg$DDICU, allow=TRUE), 
             xmlTime2POSIX(demg$DAICU, allow=TRUE),
             units=units)
    return(cbind(demg, lenstay = len))
}

#' find the unique spell ID.
#'
#' @param rec  ccRecord-class
#' @param duration integer hours 
#' @return data.table contains spell id.
#' @export unique_spell
unique_spell <- function(rec, duration=2) {
    tb <- rec@infotb
    short.time.group <- function(sd) {
        zeroday <- 0
        if (length(sd[[1]]) == 1)
            return(zeroday)
        dic <- sd$t_discharge[1:length(sd$t_discharge)-1]
        adm <- sd$t_admission[2:length(sd$t_admission)]
      
        # 0 is a mark of first episode. In order to differentiate, I added 1e-7 to all 
        # the diff days, which give an error of less than 1 mins. 
        diffday <- c(zeroday, difftime(adm, dic, units="days") + 1e-7)
        diffday[is.na(diffday)] <- 0
        diffday
    }
    setkey(tb, "pid", "t_admission", "t_discharge")
    tb[, "diffday":=short.time.group(.SD), by="pid"]

    spell <- Reduce(sum, tb$diffday == 0 | tb$diffday > duration, accumulate=TRUE)
    tb$spell <- spell
    return(tb)
}

#' Assign unique spell ID to the demographic table 
#'
#' @param rec ccRecord
#' @param duration the maximum hours of transition period
#' @return data.table demographic table with spell ID in column spell
#' 
#' @export demographic.patient.spell
demographic.patient.spell <- function(rec, duration=2) {
    dmg <- sql.demographic.table(rec)
    us <- unique_spell(rec, duration)
    us <- data.table(index=us$index, pid=us$pid, spell=us$spell)
    dmg <- merge(dmg, us, by=c("index"))
    return(dmg)
}
