load('all_patients.Rdata')
ccd <- reindex(ccd)
ccd <- deltaTime(ccd, anonymised=T)
ccd <- uniquePatient(ccd)
ccd_delta_num <- ccd
save(ccd_delta_num, file="delta_num.RData")
