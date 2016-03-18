library(testthat)
library(ccdata)

if (!exists(test_xml))
    test_xml <- xmlLoad("../CriticalCare/anon_CC.xml")
test_check("dataplay")
