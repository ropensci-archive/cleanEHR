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
        schema <- paste(find.package("ccdata"), "sql/create_table.sql", sep="/") 

    sql.init.postgres()
    cmd <- sql.load_file(schema)
    sqldf(cmd)
}

#' Add demographic table to the postgreSQL table.
#' @param record
#' @export sql.add.demographic
sql.add.demographic <- function(record) {
    sql.init.postgres()
    sqldf('DROP TABLE IF EXISTS demographic;')
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host="localhost", user="sinan", dbname="postgres")
    demogt <- sql.demographic.table(record, dtype=TRUE)
    dbWriteTable(con, "demographic", demogt)
    dbDisconnect(con)
    invisible(demogt)
}
