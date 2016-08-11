


#' find the unique spell ID. 
#' @param record ccRecord
#' @return data.table contains spell id.
#' @export unique.spell
unique.spell <- function(rec, duration=2) {
    tb <- list()
    env <- environment()
    for_each_episode(rec, 
        function(x) {
            env$tb[[length(tb) + 1]] <- 
                .simple.data.frame(list(
                    site      = as.character(x@site_id), 
                    episode_id= as.character(x@episode_id), 
                    nhs_number= as.character(x@nhs_number),
                    pas_number= as.character(x@pas_number), 
                    admt      = as.numeric(x@admin_icu_time),
                    dict      = as.numeric(x@discharge_icu_time), 
                    file_origin=as.character(x@file_origin)))
        }
    )
    tb <- rbindlist(tb, fill=TRUE)
    setnames(tb, c("site", "episode_id", "nhs_number", "pas_number", "admt",
                   "dict", "file_origin"))
    tb$dict <- suppressWarnings(as.POSIXct(as.numeric(unclass(tb$dict)),
                                           origin="1970-01-01"))
    tb$admt <- suppressWarnings(as.POSIXct(as.numeric(unclass(tb$admt)),
                                           origin="1970-01-01"))
    tb[["id"]] <- tb$nhs_number
    tb[["id"]][(tb$id=="NULL")] <- tb[["pas_number"]][tb$id=="NULL"]

    tb <- tb[!(site=="NULL" | episode_id=="NULL" | id=="NULL" | is.na(dict) | is.na(admt))]

    tb[, pid:=.GRP, by="id"]

    short.time.group <- function(sd) {
        zeroday <- 0
        if (length(sd[[1]]) == 1)
            return(zeroday)
        dic <- sd$dict[1:length(sd$dict)-1]
        adm <- sd$admt[2:length(sd$admt)]
        return(c(zeroday, difftime(adm, dic, units="days")))
    }
    setkey(tb, "pid", "admt", "dict")
    tb[, diffday:=short.time.group(.SD), by="pid"]

    spell <- Reduce(sum, tb$diffday == 0 | tb$diffday > duration, accumulate=T)
    tb$spell <- spell
    return(tb)
}



