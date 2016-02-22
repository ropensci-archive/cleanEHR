library(dataplay)
library(devtools)
reload(".")


if(!exists("ccd"))
    load("xmlData.Rdata")


plotPatientItem <- function (data.set, patient.id, item, demographic=NULL) {
    item.info <- getItemInfo(item)
    item.data <- getPatient2dItem(data.set$data2d, item.info['NHIC_code'], 1)

    item.data$val <- as.numeric(item.data$val)
    item.data <- item.data[!is.na(item.data$val), ]

    gg <- ggplot(item.data, aes(time,val))
    gg <- gg + geom_point() + geom_line()
    gg <- gg + scale_x_datetime(breaks="4 hours")
    if (!is.null(item.info['unit']) & item.info['unit'] != "NULL")
        ylabel <- paste(item, item.info['unit'])
    else 
        ylabel <- item
    gg <- gg + ylab(item)
    return(gg)
}


gg <- plotPatientItem(ccd, 1, "Spontaneous Respiratory Rate")


demography.check <- c('Height', 
                      'Weight',
                      'Date fully ready for discharge',
                      'Time fully ready for discharge',
                      'Date of ultimate discharge from ICU/HDU',
                      'Date of discharge from your hospital',
                      'Status at ultimate discharge from ICUHDU',
                      'Residence prior to admission to acute hospital',
                      'Hospital housing location (in)',
                      'Location (in)',
                      'Admission type',
                      'Treatment function code',
                      'classification of surgery',
                      'Level 2 (HDU) days',
                      'Level 3 (ICU) days',
                      'Discharge status (Reason for discharge from your unit)',
                      'Discharge location (location out)',
                      'Timeliness of discharge from your unit',
                      'Level of care at discharge from your unit',
                      'Dead or alive on discharge',
                      'Date of original admission to/attendance at acute hospital')

demgy <- data.frame(item=demography.check,
                    val=getPatient1dItem(ccd$data1d, 
                                         getItemsInfo(demography.check, 'NHIC_code'),
                                         patient.id=1))

print(paste(demgy$item, demgy$val, sep=" = "))
