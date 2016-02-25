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
