#tests
library("dataplay")
system.time(csvdata <- csv2ccd('../../CriticalCare/N_ClinicalData.csv'))
save(file="csvdata.Rdata",csvdata)
