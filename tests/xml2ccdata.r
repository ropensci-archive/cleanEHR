source("install.r")
library(dataplay)
library(devtools)
reload(".")


r <- xmlLoad("../CriticalCare/test.xml")
#ccd <-xml2Data(r, seq(20))
                #seq(,700))
xt <- system.time(ccd <- xml2Data(r))
print(xt)

save(file="xmlData.Rdata", ccd)
