#' read csv file and convert to ccdata 
#' @param file input file location.
#' @return ccdata type
csv2ccd <- function(file, ...) {
    cat("reading", file, "\n")
    csv.data <- read.csv(file, ...)
    if (dim(csv.data)[2] != 7) stop("csv.data should contain 7 columns\n")
    dup.index <- !duplicated(csv.data$itemId)
    var.id <- csv.data$itemId[dup.index]
    var.names <- paste(csv.data$Label1[dup.index], csv.data$Label2[dup.index],sep="-")
    patient.id <- unique(csv.data$episodeId)
    patient.num <- length(patient.id)

    stopifnot(length(var.id) == length(var.names))

    data.list <- list()
    cat("creating ccdata for", patient.num, "patients.\n")
    pb <- txtProgressBar(min = 1, max = length(patient.id),style = 3)
    pt.count <- 1 # for the progress bar only
    for (pt in patient.id) {
        this.patient.raw <- csv.data[csv.data$episodeId == pt, ]
        this.time <- sort(unique(this.patient.raw$charttime))
        this.itemid <- sort(unique(this.patient.raw$itemId))
        this.patient <- array("", c(length(this.time), length(this.itemid)))
        
        colnames(this.patient) <- c(paste("id", this.itemid, sep=""))
        rownames(this.patient) <- this.time
        for (t in this.time) {
            select.row.names<- 
                paste("id", this.patient.raw[this.patient.raw$charttime == t, "itemId"], sep="")
            this.patient[t, select.row.names] <-
                as.character(this.patient.raw[this.patient.raw$charttime == t, 
                             "thevalue"])
        }
        data.list[[pt]] <- this.patient
        pt.count <- pt.count + 1
        setTxtProgressBar(pb, pt.count)
    }
    cat("\n")

    return(ccData(var.id=var.id, var.names=var.names, patient.id=patient.id,
                  patient.num=patient.num, data.1d=data.list))
}
