library(RPostgreSQL)
library(sqldf)
library(assertthat)
library(ccdata)
if (!exists("ccd")) {
    load("../data/delta_num.RData")
    ccd <- ccd_delta_num
}




all.nhic.code <- function(cls) {
        data.checklist[data.checklist$Classification1 == cls,"NHICcode"]
}

code2stname <- function(code) {
    ccdata.env$code2stname.dict[code]
}

stname2code <- function(stname) {
    ccdata.env$stname2code.dict[stname]
}

short2longname <- function(stname) {
    longname <- array("NULL", length(stname))
    for (i in seq_along(stname))
        longname[i] <- ccdata.env$ITEM_REF[[stname2code(stname[i])]]$dataItem
    return(longname)
}

get.demographic.table <- function(record, dtype=TRUE) {
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



#demogt <- get.demographic.table(ccd)

#
#options(sqldf.RPostgreSQL.user ="sinan", 
#        sqldf.RPostgreSQL.dbname ="postgres",
#        sqldf.RPostgreSQL.host ="localhost",
#        sqldf.RPostgreSQL.port =5432)
#
#
#f <- file('sql/simple.sql')
#command <- paste(readLines(f), collapse='')
#close(f)
#sqldf(command)
#
###if (!exists("con"))
#
#drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", user="sinan", dbname="postgres")
dbWriteTable(con, "test2", demogt, append=T)
dbDisconnect(con)



#print(sqldf('select * from test1'))
##sqldf('drop table table_test;')
##sqldf('create table table_test')
