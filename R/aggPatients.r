##' return selected index from nhs_number or pas_number
##' @param record ccRecord
##' @param id character, either the nhs_number or pas_number
##' @return index vector 


#=============


patientIndex <- function (record, id) {
    index_nhs <- record['NULL']

#    index_pas <- record@pas_numbers[id]$index
    print(index_nhs)
    print(class(index_nhs))
 #   print(index_pas)

#    if (is.numeric(index_nhs) & is.numeric(index_pas))
#        stop('found both maching nhs number and pas number!')
#
#    if (all(is.na(c(index_nhs, index_pas))))
#        return(NA)
#    else {
#        if (is.na(index_nhs))
#            return(index_pas)
#        else
#            return(index_nhs)
#    }
}
