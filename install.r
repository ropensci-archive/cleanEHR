#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)

#create documents
if(length(args)>0){
    if(args[1] == "man"){
        if(!require(roxygen2)){
            install.packages(roxygen2)
            library(roxygen2)
        }
        roxygenize(".")
    }
}

install.packages(".", repos=NULL, type="source")
