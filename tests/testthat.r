library(testthat)
library(ccdata)

# load test data
r <- xmlLoad("../data/test_data_10_patients.xml")
ccd <<- xml2Data(r, seq(2), quiet=TRUE)

test_check("ccdata")
