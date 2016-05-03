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
#' @export for_each_patient
for_each_patient <- function(record, fun) {
    lapply(record@patients, fun)
}

#' @export for_each_episode_data
for_each_episode_data <- function(record, fun) {
    env <- environment()
    np <- 0
    lapply(record@patients, 
           function(p) {
               print(env$np)
               env$ne <- 0
               env$np <- env$np + 1
               lapply(p@episodes, 
                      function(e) {
                          env$nd <- 0
                          env$ne <- env$ne + 1
                          lapply(e@data, 
                                 function(d) {
                                     env$nd <- env$nd + 1
                                     fun(d, env$np, env$ne, env$nd)
                                 })
                      })
           })
}


#' @export for_each_episode_data2
for_each_episode_data2 <- function(record, fun) {
    lapply(record@patients, 
           function(p) {
               lapply(p@episodes, 
                      function(e) {
                          lapply(e@data, 
                                 function(d) {
                                     return(fun(d))
                                 })
                      })
           })
}
