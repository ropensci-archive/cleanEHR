# 
library(ccdata)
if (!exists("ccd_delta_num"))
    load('../data/delta_num.Rdata')
config <- 'tests/data/ANALYSIS_REF.yaml'

dt <- create2dclean(ccd_delta_num, config, nchunks=5)

