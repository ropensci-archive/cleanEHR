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
    l.stnames <- unlist(lapply(cleanEHR:::ITEM_REF, 
                  function(x) {
                      if(!is.null(x$NHICdtCode))
                          return(x$shortName)
                  }))
    names(l.stnames) <- l.stnames # the result can be indexed via short names
    return(lapply(l.stnames, function(x) rec2tb(ccd, x)))
}

#' @import dplyr
#' @export
sql.create.database <- function(ccd, path="cchic.sqlite3") {
    unlink(path)
    cchic_db <- src_sqlite(path, create = T)
    ltb <- export.lontb(ccd)

    for (i in seq(ltb)) {
        data = ltb[[i]] # copy_to does not like [[]] operators for df.
        name = names(ltb[i])
        if (nrow(data) != 0)
            copy_to(dest=cchic_db, df=data, name=name, temporary = FALSE)
    }
    tb <- suppressWarnings(as.data.frame(sql.demographic.table(ccd)))
    copy_to(dest=cchic_db, df=tb, name="episodetb", temporary = FALSE)

    cchic_db
}


#' @export
sql.tbl.vartb <- function(con, stname) {
    tbl(con, sql(paste("select * from", stname)))
}


#' @export 
sql.collect.vartb <- function(con, stname) {
    return(collect(sql.tbl.vartb(con, stname), n = Inf))

}


#' @export 
create.fat.table <- function(db, frequency=1) {
    eptb <- data.table(sql.collect.vartb(db, 'episodetb'))
    eptb <- data.table(lenstay(eptb))
    nep0 <- nrow(eptb)
    eptb <- eptb[!is.na(lenstay)]
    eprm.log(nep0, nrow(eptb), "unable to calculate length of stay.")
    eptb <- eptb[, c("ICNNO", "ADNO", "lenstay"), with=FALSE]
    names(eptb) <- c("site_id", "episode_id", "lenstay")
    
    h <- data.table(sql.collect.vartb(db, 'h_rate'))
    h$episode_id <- as.integer(h$episode_id)
    grptb <- h[, .GRP, by=c("site_id", "episode_id")]
    grptb <- merge(grptb, eptb, all.x=TRUE, by=c("site_id", "episode_id"))
    grpindex <- vector()
    grpindex[grptb$GRP] <- as.numeric(grptb$lenstay)
    h <- h[, reallocateTime(.SD, grpindex[.GRP], 1), by=c("site_id", "episode_id")]


    return(h)
}


#' @export 
eprm.log <- function(nep0, nep1, reason) {
    cat("[-]", 
        nep0 - nep1, 
        "episodes have been removed:", 
        reason, "\n")
}



