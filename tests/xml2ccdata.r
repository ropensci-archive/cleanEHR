source("install.r")
library(dataplay)
library(devtools)
reload(".")


#r <- xmlLoad("../CriticalCare/test.xml")
#cc <-xml2Ccdata(r, seq(1,5))
cc <-xml2Ccdata(r, seq(672,700))
                #seq(,700))

#pd1<-patientToDataArray(getXmlPatient(r,2), extractInfo()$time, extractInfo()$meta)



#save(file="xmlData.Rdata", data)
