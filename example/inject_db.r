library(ccdata)
if (!exists("ccd")) {
    load("../data/delta_num.RData")
    ccd <- ccd_delta_num
}


sql.newdb()
sql.add.demographic(ccd)
