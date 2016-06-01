#!/usr/bin/Rscript

library(ccdata)

args = commandArgs(trailingOnly=TRUE)
r <- xmlLoad(args[1])
npatient <- length(names(r[[1]][[2]]))

step_iterator <- seq(1, npatient, by=100)
steps <- c(step_iterator, npatient+1)


for (i in seq(step_iterator)) {
    cat("extract from", c(steps[i], "to", steps[i+1]-1, "\n"))
    ccd <- xml2Data(xml=r, file_origin=args[1], select.episode=seq(steps[i], steps[i+1]-1), quiet=FALSE)
    save(ccd, file=paste(args[1], steps[i], steps[i+1]-1, ".RData", sep="_"))
}
