library(testthat)
library(dataplay)

if (!exists(test_xml))
    test_xml <- xmlLoad("../CriticalCare/anon_CC.xml")
test_check("dataplay")
