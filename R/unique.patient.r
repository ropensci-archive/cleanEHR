#' Aggregate episodes with the same patient ids (e.g. nhs_number or pas_number)
#' @export uniquePatients
uniquePatients <- function(record) {

    nhs <- record@nhs_numbers
    pas <- record@pas_numbers
    setkey(nhs, nhs_numbers)
    setkey(pas, pas_numbers)

    # primarily using NHS number; using PAS number while NHS number is not
    # presented.  
    nhs["NULL"] <- pas[index==nhs["NULL"]$index]
    newid <- data.table(index=nhs$index, id=nhs$nhs_numbers)
    setkey(newid, id)

    newrc <- ccRecord()
    for (id in unique(newid)$id) {
        newp <- ccPatient()
        # NOTE: multiple episodes case? improvement required.
        for (dupid in newid[id]$index) { 
            newp <- newp + record@patients[[dupid]]@episodes[[1]]
        }
        newrc@patients[[length(newrc@patients) + 1]] <- newp 
    }
    return(reindexRecord(newrc))
}
