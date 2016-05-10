library(ccdata)
if(!exists("ccd"))
load("../data/ccRecord.Rdata")
#    stop("we need ccd record - propagated numeric time record")



missmesg <- function(text, miss, all=NULL) {
    if(is.null(all))
        return(paste(text, miss, "\n"))
    else
        return(paste(text, miss,"/", all, " [", round(miss/all*100, digits=2),
                      "%]\n", sep=""))
}

missmesg2 <- function(miss, all) {
    if(is.na(miss/all))
        return("")
    else
        return(paste(miss,"/", all, " [", round(miss/all*100, digits=2),
                      "%]", sep=""))

}

for_each_unit_year <- function(dt, val=TRUE) {
    stopifnot(class(dt)[1] == "data.table")
    dt <- data.table(dt, year=round(dt$episode_id/10000))
    years <- unique(dt$year)
    sites <- unique(dt$site)
    result <- array("NA", c(length(years), length(sites)))
    colnames(result) <- sites
    rownames(result) <- as.character(years)

    for (s in sites) {
        for (y in years) {
            sub <- dt[site_id == s & year == y]
            result[which(years==y), s]<- missmesg2(nrow(sub[val!=TRUE]),
                          nrow(sub))
        }
    }
    return(result)
}

write.md <- function(tb) {
    txt <- "| year |"
    txt <- paste(txt, paste(colnames(tb), collapse="|"), "|\n")
    line <- paste(rep("------", ncol(tb)+1), collapse="|")
    line <- paste("|", line, "|\n", sep="")
    txt <- paste(txt, line, sep="")
    for(i in seq(nrow(tb))) {
        txt <- paste(txt, "|", rownames(tb)[i], sep="")
        for(j in seq(ncol(tb))){
            txt <- paste(txt, "|", tb[i,j], sep="")
        }
        txt<-paste(txt, "|\n", sep="")
    }
    return(paste(txt, "\n"))

}
#=================================
cat('General Summary\n')
cat('--------------\n')
#=================================
site_id <- unlist(for_each_episode(ccd, function(x) x@site_id))
episode_id <- unlist(for_each_episode(ccd, function(x) x@episode_id))
episode_id[episode_id=="NULL"] <- '0' # assign 0 to NULL value
episode_id <- as.numeric(episode_id)


npatient <- ccd@npatient
nepisode <- length(episode_id)
num_miss_episode_id <- length(which(episode_id == 0))


cat('Number of patients: ', npatient, '\n')
cat('Number of episodes: ', nepisode, '\n')
cat(missmesg('Episodes without episode_id: ', num_miss_episode_id, nepisode))


cat('\n')
#=================================
cat('Missing rate of 1D (demographic) data\n')
cat('--------------\n')
#=================================
print_summary <- function(dt) {

}
nhic_1d <-
    as.character(data.checklist$NHICcode[
                 data.checklist$Classification1!="Demographic"])


for (i in nhic_1d) {
    have_data <- unlist(for_each_episode(ccd, function(ep) 
                                  have_data=any(names(ep@data) == i)))
    have_data <- data.table(site_id, episode_id, val=have_data)
    stopifnot(nrow(have_data) == nepisode)
    result <- for_each_unit_year(have_data)

    cat("\n*", ccdata.env$ITEM_REF[[i]]$dataItem, ": ")
    cat(missmesg("", nrow(have_data[val!=TRUE]), nrow(have_data)))
    cat("\n")
#    cat(write.md(result))
}

