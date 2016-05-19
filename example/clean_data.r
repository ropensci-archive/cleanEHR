# 
library(ccdata)

if (!exists("ccd_delta_num"))
    load('../data/delta_num.Rdata')

# 
#if (!exists("cdt"))
    cdt <- (new.ccDataTable(ccd_delta_num, "tests/data/test_yml.yml"))
