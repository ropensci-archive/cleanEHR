#' find the unique spell ID. 
#' @param record ccRecord
#' @return data.table contains spell id.
#' @export unique.spell
unique.spell <- function(rec) {
    tb <- list()
    env <- environment()
    for_each_episode(rec, 
                     function(x) {
                         env$tb[[length(tb) + 1]] <- 
                             .simple.data.frame(list(
                                                     site=x@site_id, 
                                                     episode_id=x@episode_id, 
                                                     nhs_number=x@nhs_number,
                                                     pas_number=x@pas_number, 
                                                     admt=x@admin_icu_time,
                                                     dict=x@discharge_icu_time, 
                                                     parse_time=x@parse_time,
                                                     file_origin=x@file_origin))
                     })
    tb <- rbindlist(tb, fill=TRUE)
    setnames(tb, c("site", "episode_id", "nhs_number", "pas_number", "admt",
                   "dict", "parse_time", "file_origin"))
    tb$dict <- suppressWarnings(as.POSIXct(as.numeric(unclass(tb$dict)),
                                          origin="1970-01-01"))
    tb$admt <- suppressWarnings(as.POSIXct(as.numeric(unclass(tb$admt)),
                                          origin="1970-01-01"))
    tb[["pid"]] <- tb$nhs_number
    tb[["pid"]][(tb$pid=="NULL")] <- tb[["pas_number"]][tb$pid=="NULL"]

    tb <- tb[!(site=="NULL" & episode_id=="NULL")]
    tb[, spell:=.GRP, by="pid"]
    setkey(tb, "admt")
    tb

}



