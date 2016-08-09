library(ccdata)
# load

select_data <- c("Heart rate", "PaO2/FiO2 ratio", "Sex", "Admission type", "Date
                of discharge from your hospital")
                
table <- ccRecord2Table(ccd, "NIHR_HIC_ICU_0108")
