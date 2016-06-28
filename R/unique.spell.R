#' find the unique spell ID. 
#' @param record ccRecord
#' @return data.table contains spell id.
#' @export unique.spell
unique.spell <- function(record) {
    env <- environment()
    tb <- list()
    for_each_episode(record, 
                     function(x) {
                         env$tb[[length(tb)+1]] <-
                             .simple.data.frame(list(site=x@site_id,
                                                     episode_id=x@episode_id,
                                                     nhs_num=x@nhs_number,
                                                     pas_num=x@pas_number,
                                                     admt=x@admin_icu_time,
                                                     dict=x@discharge_icu_time))
                     })
    tb <- rbindlist(tb)
    tb <- tb[!(site=="NULL" & episode_id=="NULL")]
    tb[, patient_id:=.GRP, by=c("site", "episode_id")]
    tb
}
