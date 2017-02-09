# This script is to address the ccdata and cleanEHR data object
# incompatible problem. You need both ccdata and cleanEHR being installed.  


library(ccdata)
file <- (commandArgs(TRUE)[1])
load(file)
data_obj <- ls()[1]



new_obj <- cleanEHR::ccRecord() + 
    for_each_episode(eval(parse(text = data_obj)), 
                     function(x) {
                         cleanEHR::new.episode(x@data)
                     })


assign(data_obj, new_obj)

file_name <- strsplit(file, "[.]")[[1]][1]
save(file=paste0(file_name, "_v2.RData"), list=data_obj)
