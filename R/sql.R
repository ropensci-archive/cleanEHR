#' Extract a variable from ccRecord object to a table.
#' 
#' The output table will contain site_id, episode_id, time, val, and possible
#' meta data column if the variable is longitudinal. Otherwise only site_id,
#' episode_id and the value will be presented. 
#' @param ccd ccRecord object 
#' @param varname the short name of the selected variable, see ITEM_REF
#' @export 
rec2tb <- function(ccd, varname) {

    varcode <- stname2code(varname)
    if (is.na(names(stname2code(varname)))) 
        stop("The short name ", varname, " cannot be found.")

    vartb.lst <- 
        for_each_episode(ccd, 
        function(x) {
            if (is.null(x@data[[varcode]]))
                return(NULL)
            else 
                return(cbind(.simple.data.frame(
                      list(episode_id=x@episode_id, 
                           site_id=x@site_id)),
                      x@data[[varcode]]))
        })
    vartb <- rbindlist(vartb.lst, fill=TRUE)
    nm <- names(vartb)

    # Rename the columns 
    # The sequence of time and item2d column is not certain, that's why we have
    # to search the index of item2d first. 
    if (ncol(vartb) == 3) # non-longitudinal variables
        names(vartb) <- c("episoe_id", "site_id", varname)
    if (ncol(vartb) > 3) # longitudinal 
        names(vartb) <- gsub(x=nm, "item2d", "val")
    # two val may happen for microb site. 
    if (any(duplicated(nm))) {
        nm[duplicated(nm)] <- paste0(nm[duplicated(nm)], 1)
        names(vartb) <- nm
    }
    return(vartb)
}


#' Export all the longitudinal fields as tables 
#'
#' All the result tables will be wrapped by a list, indexing with short names of
#' the fields. 
#' @param ccd ccRecord object
#' @export
export.lontb <- function(ccd) {
    l.stnames <- code2stname(names(ITEM_REF)[is.longitudinal(names(ITEM_REF))])
    names(l.stnames) <- l.stnames # the result can be indexed via short names
    return(lapply(l.stnames, function(x) rec2tb(ccd, x)))
}


#' @importFrom dplyr src_sqlite copy_to
#' @import RSQLite
#' @export
sql.create.database <- function(ccd, path="cchic.sqlite3") {
    unlink(path)
    cchic_db <- src_sqlite(path, create = T)
    ltb <- export.lontb(ccd)

    for (i in seq(ltb)) {
        data = ltb[[i]] # copy_to does not like [[]] operators for df.
        name = names(ltb[i])
        if (nrow(data) != 0) {
            copy_to(dest=cchic_db, df=data, name=name, temporary = FALSE)
        }
        else 
            warning("cannot find any data of ", name, "in ccd.")
    }
    tb <- suppressWarnings(as.data.frame(sql.demographic.table(ccd)))
    copy_to(dest=cchic_db, df=tb, name="episodetb", temporary = FALSE)

    cchic_db
}

#' @importFrom dplyr sql
#' @export
sql.tbl.vartb <- function(con, stname) {
    tbl(con, sql(paste("select * from", stname)))
}


#' @importFrom dplyr collect tbl
#' @export 
sql.collect.vartb <- function(con, stname) {
    return(collect(sql.tbl.vartb(con, stname), n = Inf))
}

#' Fat table 
#' 
#' We should be able to give the users choices of how to align the data, whether by 
#' using ICU admission time as the baseline, or hospital admission, or even with the earliest 
#' and the latest data point. I have updated reallocateTime__ which allows such 
#' operations. 

#' @export 
create.fat.table <- function(db, frequency=1) {
    tbindb <- src_tbls(db)
    # clean up fat table and tmp table in the database  
    for (i in c("ftable", "tmp")) {
        if (i %in% tbindb)
            db$con %>% db_drop_table(table=i)
    }

    eptb <- data.table(sql.collect.vartb(db, 'episodetb'))
    eptb <- data.table(lenstay(eptb))
    nep0 <- nrow(eptb)
    eptb <- eptb[!is.na(lenstay)]
    eprm.log(nep0, nrow(eptb), "unable to calculate length of stay.")
    eptb <- eptb[, c("ICNNO", "ADNO", "lenstay"), with=FALSE]
    names(eptb) <- c("site_id", "episode_id", "lenstay")

    fatbasetb <- eptb[, seq(0, ceiling(as.numeric(lenstay))), by=c("site_id", "episode_id")]
    names(fatbasetb) <- c("site_id", "episode_id", "time")
    copy_to(db, fatbasetb, 'fattb', temporary=FALSE)

#    lonvars <- c('adrenaline', 'advsupt_cardv', "h_rate")
    lonvars <- names(ITEM_REF)[is.longitudinal(names(ITEM_REF))]
    lonvars <- code2stname(lonvars)
    
    if (any(!(lonvars %in% tbindb)))
        warning(paste(lonvars[!(lonvars %in% tbindb)], collapse=", "),
                      ", cannot be found in the database.")
    lonvars <- lonvars[lonvars %in% tbindb]
    
    lst <- list()

    for (l in lonvars) {
        print("===================================================")
        print(l)
        var <- data.table(sql.collect.vartb(db, l))
        print(var)
        var <- var[, alignTime(.SD, 0, max(time), 1), by=c("site_id", "episode_id")]

        # name val -> stname, meta -> stname.meta
        nm <- names(var)
        nm[nm == "val"] <- l
        nm[nm == "meta"] <- paste0(l, ".meta")
        names(var) <- nm

        copy_to(db, var, 'vartb', temporary=FALSE)
        print(names(db))
        left.join.var.table(db$con)


        db$con %>% db_drop_table(table="tmp")
        print(var)
#
#        h$episode_id <- as.integer(h$episode_id) # episode id need to be integer, should convert the datatype in create.database. 
#        grptb <- h[, .GRP, by=c("site_id", "episode_id")]
#
#        grptb <- merge(grptb, eptb, all.x=TRUE, by=c("site_id", "episode_id"))
#        grpindex <- vector()
#        grpindex[grptb$GRP] <- as.numeric(grptb$lenstay)
#        hf <- h[, reallocateTime(.SD, grpindex[.GRP], 1), by=c("site_id", "episode_id")]
    }


}


#' Join the existing vartb and fattb and create update the new fattb
#' 
#' Update the fattb and the vartb will be removed here.
#' @param dbname character database address
left.join.var.table <- function(dbname) {
    con <- dbConnect(drv = SQLite(), dbname=dbname)
    # Get the exact column names to avoid duplications in join, such as site_id.1
    basecolname <- names(dbGetQuery(con, "select * from fattb limit 1;"))
    varcolname <- names(dbGetQuery(con, "select * from vartb limit 1;"))
    varcolname <- varcolname[-which(varcolname %in% c('site_id', 'episode_id', 'time'))]
    newcolname <- c(paste0("fattb.", basecolname), varcolname)

    q.jointable  <- 
        paste0("CREATE TABLE tmp as SELECT ", paste(newcolname, collapse=", "), 
               " FROM fattb LEFT OUTER JOIN vartb ON 
               fattb.site_id = vartb.site_id and 
               fattb.episode_id = vartb.episode_id and 
               fattb.time = vartb.time;")

    dbSendQuery(con, q.jointable)
    dbSendQuery(con, "DROP TABLE fattb;")
    dbSendQuery(con, "ALTER TABLE tmp RENAME TO fattb;")
    dbSendQuery(con, "DROP TABLE vartb;")

    dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)
}

#' @export 
eprm.log <- function(nep0, nep1, reason) {
    cat("[-]", 
        nep0 - nep1, 
        "episodes have been removed:", 
        reason, "\n")
}



