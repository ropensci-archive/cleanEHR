#' @import parallel
NULL

find.new.xml.file <- function(xml.path, restart=FALSE) {
    rdata.path <- paste(xml.path, ".database", sep="/")
    dir.create(rdata.path, showWarnings=F)

    xml.file.name <- dir(xml.path)

    if (! all(grepl("(.xml|.XML)$|.xml_", xml.file.name))) 
        stop("file names in xml.path ", xml.path, " must contain the key word .xml. ") 

    parsed.pattern <- unique(sapply(strsplit(dir(rdata.path), ".xml"), function(x) x[1]))
    if (length(parsed.pattern) == 0 | restart == TRUE) {
        return(xml.file.name)
    } else {
        all.xml.files <- 
            xml.file.name[!grepl(paste(parsed.pattern, collapse="|"), xml.file.name)]
        return(all.xml.files)
    }
}


#' @title update the RData database
#' @description Detect the new XML files which has never been parsed first and
#' inject the new episode data to the .RData database. 
#' @param xml.path 
#' @param restart 
#' @param mc.cores 
#' @return ccRecord object
update.new.xml <- function(xml.path, restart=FALSE, mc.cores=4) {
    cat("\nupdating new XML files\n")
    files.to.parse <- find.new.xml.file(xml.path, restart)

    db.collection <- mclapply(files.to.parse, 
                              function(x) {
                                  fxml <- paste(xml.path, x, sep="/")
                                  frdata <- paste(xml.path, "/.database/", x, ".RData", sep="")
                                  db <- xml2Data(fxml, quiet=F)
                                  save(db, file=frdata)
                                  return(db)
                              }, mc.cores=mc.cores)

    #' combine new data to a ccRecord 
    new.db <- ccRecord()
    for (i in seq(db.collection)) {
        new.db <- new.db + db.collection[[i]]
    }
    return(new.db)
}


#' @title updating the database 
#' @description something 
#' @details 
#' @export update.database
update.database <- function(xml.path, restart=FALSE, split=FALSE, 
                            mc.cores=4, save=TRUE) {
    alldata.loc <- paste(xml.path, "alldata.RData", paste="/")

    if (exists(alldata.loc)) 
        load(alldata.loc) # the old database has a variable name alldata
    else 
        alldata <- ccRecord()

    new.db <- update.new.xml(xml.path, restart, mc.cores)
    alldata <- alldata + new.db

    if (save) 
        save(alldata, file=paste(xml.path, ".database", "alldata.RData", sep="/"))
    
    invisible(alldata)
}


break.down.xml <- function(xml.path) {
    dir.creaet(paste(xml.path, ".origin"))

}
