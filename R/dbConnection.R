#' @import magrittr
#' @import dplyr
NULL

#' Make a connection to the Database that can be used for queries
#'
#' @export
connect <- function(host='localhost', username=NULL, password=NULL, database=NULL){
    DBI::dbConnect(RPostgreSQL::PostgreSQL(), host=host, user=username, password=password, dbname=database)
}

#' The metadata table. This is all of the different 
#' codes that are available with their meanings, and
#' what the different columns mean for them. 
#'
#' This functions returns a loaded into memory R table,
#' **not** a connection to database table.
#'
#' @export
metadata   <- function(connection){
    if(missing(connection)){
        stop("A connection must be provided")
    }
    dplyr::tbl(connection, "variables") %>% dplyr::collect()
}

#' Return all the the recorded data.
#' This is a connection to a database / query,
#' not a real value. If you want to use it you will
#' need to run collect() on it.
#'
#' @export
allData <- function(connection){
    if(missing(connection)){
        stop("A connection must be provided")
    }
    # Create connections to all the relevant tables
    provenance <- dplyr::tbl(con, "provenance")
    episodes   <- dplyr::tbl(con, "episodes")
    events     <- dplyr::tbl(con, "events")

    # Join them correctly
    provenance %>%
        left_join(episodes, by=c("file_id" = "provenance")) %>%
        left_join(events, by=c("episode_id"))
}

#' Return all the the recorded data but only fields users are allowed to see.
#' This is a connection to a database / query,
#' not a real value. If you want to use it you will
#' need to run collect() on it.
#'
#' @export
exportData <- function(connection){
    if(missing(connection)){
        stop("A connection must be provided")
    }
    # Create connections to the events table
    events     <- dplyr::tbl(con, "events")
    return(events)
}

#' Turn a table in a ccdata object. You do that basically using the follwing steps:
#' con <- connect(...)
#' ccd <- table.to.ccdata(exportData(con) %>% collect, metadata(connection = con))
#'
#' @export
table.to.ccdata <- function(table, mdata) {
  table %>%
    split(., .$episode_id) %>%
    lapply(function(episode.table){
      episode.table %>% 
        split(., .$code_name) %>%
        lapply(function(record.type){
          cname = record.type$code_name %>% unique
          types = mdata %>% 
            filter(code_name == cname) %>%
           `[`(., sapply(., function(x) !is.na(x)) %>% as.vector) %>%
            select(-code_name, -long_name, -primary_column) %>% 
            names
          
          if(length(types) == 1) {
              if(cname == 'NIHR_HIC_ICU_0005'){
                  return(as.character(record.type[[types]]))
              }
            return(record.type[[types]])
          }
          
          data = record.type[types]
          primary_column = mdata %>%
            filter(code_name == cname) %>%
            extract2('primary_column') %>% 
            unique

          types[types == 'datetime'] = 'time'
          types[types == primary_column] = 'item2d'
          types[types != 'time' & types != 'item2d'] = 'meta'

          data = setNames(data, types)

          if('time' %in% types){
              data %<>% 
                mutate(time = as.numeric(time))
          }

          data
        }) %>%
        new.episode
    }) %>% (function(eps) { ccRecord() + eps} )
}

