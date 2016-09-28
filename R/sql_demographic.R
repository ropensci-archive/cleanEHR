#' Create demographic SQL tables. The data type of each column is in its
#' corresponding data type.
#' @export sql.demographic.table
sql.demographic.table <- function(record, dtype=TRUE) {
    env <- environment()
    demogls <- list()
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
    demogt <- rbindlist(demogls)
    setnames(demogt, code2stname(names(demogt)))

    if (dtype) {
        for (i in seq(ncol(demogt))){
            demogt[[i]] <- 
                .which.datatype(stname2code(names(demogt)[i]))(demogt[[i]])
        }
    }
    return(demogt)
}


#' Initialise the connection of between sqldf and postgres.
#' @export sql.init.postgres
sql.init.postgres <- function() {
    options(sqldf.RPostgreSQL.user ="sinan", 
            sqldf.RPostgreSQL.dbname ="postgres",
            sqldf.RPostgreSQL.host ="localhost",
            sqldf.RPostgreSQL.port =5432)
}

#' Load SQL command from a file in which the SQL commands are recorded.
#' @param path the full path of the SQL file.
#' @return a string of command.
#' @export sql.load_file
sql.load_file <- function(path) {
    sqlfile <- file(path)
    cmd <- paste(readLines(path), collapse="")
    close(sqlfile)
    return(cmd)
}

#' Remove all the existing tables from the database and load the new schema
#' file.
#' @param schema is the full path of the selected schema file.
#' @export sql.newdb
sql.newdb <- function(schema=NULL) {
    if (is.null(schema))
        schema <- paste(find.package("ccdata"), "inst/sql/create_table.sql", sep="/") 

    sql.init.postgres()
    cmd <- sql.load_file(schema)
    sqldf(cmd)
}

#' Add demographic table to the postgreSQL table.
#' @param record
#' @export sql.add.demographic
sql.add.demographic <- function(record, update=FALSE) {
    sql.init.postgres()
    if (update)
        sqldf('DROP TABLE IF EXISTS demographic;')
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host="localhost", user="sinan", dbname="postgres")
    demogt <- sql.demographic.table(record, dtype=TRUE)
    dbWriteTable(con, "demographic", demogt)
    dbDisconnect(con)
    invisible(demogt)
}


episode.long.table <- function(ep) {
    eid <- paste(ep@site_id, ep@episode_id, sep="_")
    episode_data <- list()
    for(fname in names(ep@data)) {
        field <- ep@data[[fname]]
        nrows <- nrow(field)
        if (!is.null(nrows)) {#2d data
            d <- list()
            stopifnot(!is.null(field$time))
            stopifnot(!is.null(field$item2d))
            d[["eid"]] <- rep(eid, nrows)
            d[["field"]] <- rep(fname, nrows)
            d[["time"]] <- field$time
            d[["value"]] <- field$item2d
            d[["meta"]] <- field$meta
            if (is.null(d[["meta"]]))
                d[["meta"]] <- rep(NA, nrows)
            episode_data[[length(episode_data) + 1]] <- .simple.data.frame(d)
        }
    }
    return(rbindlist(episode_data))
}

#'
#' @export sql.add.episode
sql.add.episode <- function(record, update=FALSE) {
    sql.init.postgres()
    tb <- "timedata"
    if (update)
        sqldf(paste('DROP TABLE IF EXISTS', tb))
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host="localhost", user="sinan", dbname="postgres")
    for_each_episode(record, 
        function(ep)
            dbWriteTable(con, tb, episode.long.table(ep), append=T))
}
