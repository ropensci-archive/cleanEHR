#!/usr/bin/Rscript

library(ccdata)

args = commandArgs(trailingOnly=TRUE)

load(args[1])
ccd <- reindex(ccd)
ccd <- deltaTime(ccd, anonymised=T)
ccd <- uniquePatient(ccd)
ccd_delta_num <- ccd
save(ccd_delta_num, file=args[2])
