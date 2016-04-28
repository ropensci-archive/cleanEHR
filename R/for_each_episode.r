#' @export for_each_episode
for_each_episode <- function(record, fun) {
    lapply(record@patients, 
           function(p) {
               lapply(p@episodes, 
                      function(e) {
                          fun(e)
                      })
           })
}


#' @export update_record
update_record <- function(record, dlist, fun) {
    env <- environment()
    

}
