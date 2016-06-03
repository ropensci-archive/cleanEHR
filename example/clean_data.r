# 
library(ccdata)

if (!exists("ccd"))
    load('../data/delta_num.Rdata')

# 
if (!exists("cdt"))
    cdt <- (new.ccDataTable(ccd_delta_num, "tests/data/test_yml.yml"))

cdt_no_null <- remove_null(cdt)
