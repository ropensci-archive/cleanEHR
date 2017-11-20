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

#' Lookup codes based by keywords
#' 
#' This function tries to match keywords.
#' The matched items will be displayed. 
#' @param metadata The metadata table. This must be the actual object (collect()ed, not a db connection)
#' @param keyword character e.g. "heart", "108". 
#'          If you provide multiple keywords it will try match all of them.
#' @return A subset of the metadatatable which contains only rows which have the given keyword.
#' @examples
#' lookup.items('heart')
#' lookup.items(c('admission', 'unit'))
#' @export 
lookup.items <- function(metadata, keywords) {
    # Just search based on regex for now.
    Reduce(function(table, k) filter_all(table,any_vars(grepl(k,., ignore.case=TRUE))),
           keywords, metadata)
}


#' Get a list of columns that count as demographics
#'
#' @export
demographic.codes <- function(){
    data_columns <- metadata %>%
        select(-code_name,-long_name,-primary_column) %>%
        colnames
    table = metadata %>%
        dplyr::collect
    table %>% 
        mutate (nas = table %>% 
            select(data_columns) %>% 
            as.data.table %>%
            apply(1, function(x) sum(!is.na(x)))) %>% 
        filter(nas == 1) %>% 
        select(code_name)
}
#' This gets you the demographics as a table out of your current data.
#'
#' @export
demographics.table <- function(table) {
    table %>% 
        inner_join(demographic.codes(), by="code_name")
}


range.to.function <- function(range){
    # A regex for a number. This is less permissive than R's numbers.
    # This is intentional to avoid numbers which may be easy to misread.
    # I.e. .8 (must be 0.8), and 5. (must be 5) which would be accepted by R
    # Matches:
    #  * initial optional +/- 
    #  * mandatory digit (must have leading 0 on decimals)
    #  * optional decimal point with following numbers
    #  * optional exponent with optional sign, and mandatory digits
    number_regex = "([+-]?\\d+(.\\d+)?(e[+-]?\\d+)?)"
    open_range_regex = "([[(])"
    close_range_regex = "(\\)|])"
    sep_range_regex = ","
    space = "\\s*"
    range_regex = paste0('^',open_range_regex,space,number_regex, space, sep_range_regex, space, number_regex, space, close_range_regex)

    if(!grepl(range_regex, range)){
        stop('Range is not a valid range pattern')
    }
    start = regexpr(open_range_regex, range) %>% regmatches(range, .)
    close = regexpr(close_range_regex, range) %>% regmatches(range, .)
    halves = range %>% strsplit(",") %>% unlist
    numbers = regexpr(number_regex, halves) %>% regmatches(halves, .)

    lhs = as.numeric(numbers[1])
    rhs = as.numeric(numbers[2])
    
    func = paste0('compare',start,close)
    
    f <- get(func)

    partial(f, lower=lhs, upper=rhs) 
}

#--- Private functions for range.to.function

`compare()` <- function(lower, upper, test) {
    return(lower < test & test < upper)
}

`compare[)` <- function(lower, upper, test) {
    return(lower <= test & test < upper)
}

`compare(]` <- function(lower, upper, test) {
    return(lower < test & test <= upper)
}

`compare[]` <- function(lower, upper, test) {
    return(lower <= test & test <= upper)
}

#' Turn a table in a ccdata object
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
  
          setNames(data, types)
        }) %>%
        new.episode
    }) 
}

#con <- connect(username="roma", database= "roma")
#e <- exportData(con)
#table.to.ccdata(e %>% collect, metadata(connection = con))

