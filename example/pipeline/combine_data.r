#!/usr/bin/Rscript
library(ccdata)

institute <- c("CUH", "Oxford", "GSTT", "imperial", "UCLH")
#institute <- "imperial"

data<-dir(institute, full.name=TRUE)[grep(".RData", dir(institute,
                                                        full.name=TRUE))]

new <- ccRecord()
for (d in data) {
    print(d)
    load(d)
    new <- new + ccd 
}

ccd <- new

save(ccd, file="all_patients.Rdata")
