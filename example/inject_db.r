library(ccdata)
if (!exists("ccd")) {
    load("../data/delta_num.RData")
    ccd <- ccd_delta_num
}


sql.newdb()
tb <- sql.add.demographic(ccd)
