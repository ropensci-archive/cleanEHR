#' select one data item from ccRecord by unit by time
#' @export select_data
select_data <- function(record, item, sites=NULL, years=NULL, propagate=FALSE,
                        frequency=NULL) 
{
    if(is.null(frequency) & !propagate)
        stop("specify frequency if you want to propagate the variable")

    env <- environment()
    result <- list()

    for_each_episode(record, 
                     function(ep) {
                         # checking year and site id in the selecting list 
                         if (!is.null(years)) {
                             if (ep@episode_id == "NULL")
                                 epy <- "NULL"
                             else
                                 epy <- round(as.numeric(ep@episode_id)/10000)
                             if (epy %in% years) in_year <- TRUE
                             else in_year <- FALSE
                         } else 
                             in_year <- TRUE

                         if (!is.null(sites)) {
                             if (ep@site_id %in% sites) in_site <- TRUE
                             else in_site <- FALSE
                         } else 
                             in_site <- TRUE

                         # copy data out
                         if (in_site & in_year) {
                             if (propagate == TRUE) {
                                 if (length(ep@data[[item]]) > 1) {
                                     period_length <- getEpisodePeriod(ep)
                                     tryCatch(env$result[[length(env$result) + 1]] <- 
                                         reallocateTime(ep@data[[item]],
                                                        period_length,
                                                        frequency),
                                              error=function(e) {
                                                  print(ep@data[[item]]); stop()
                                              })
                                 }
                             } else
                                 env$result[[length(env$result) + 1]] <- ep@data[[item]]
                         }

                     })

    return(result)
}
