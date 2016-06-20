#' pick up the identical episode which has been injected by mistake.
#' @export duplicated.episode 
duplicated.episode <- function(cr) {
    nhs_number <- unlist(for_each_episode(cr, function(x) x@nhs_number))
    pas_number <- unlist(for_each_episode(cr, function(x) x@pas_number))
    site <- unlist(for_each_episode(cr, function(x) x@site_id))
    episode_id  <- unlist(for_each_episode(cr, function(x) 
                                          x@episode_id))

    adm <- unlist(for_each_episode(cr, function(x)
                                   x@data[[ccdata.env$code_admin_icu_t]]))
    disc <- unlist(for_each_episode(cr, function(x) 
                                   x@data[[ccdata.env$code_discharge_icu_t]]))

 # return(paste(site, episode_id, sep="_"))  
    return(paste(nhs_number, pas_number, site, episode_id, adm, disc, sep="_"))
}
