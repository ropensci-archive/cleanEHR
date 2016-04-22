#' Aggregate episodes with the same patient ids (e.g. nhs_number or pas_number)
#' @export combineEpisode
combineEpisode <- function(record) {
    nhs.duplicated.index<-
        record@nhs_number[duplicated(ccd@nhs_number[!'NULL'])]$index
    pas.duplicated.index <-
        record@pas_number[duplicated(ccd@pas_number[!'NULL'])]$index

    duplicated.index <- sort(unique(as.integer(c(nhs.duplicated.index,
                                                 pas.duplicated.index))))

    for(id in duplicated.index) {
        for (ep in record@patients[[id]]@episodes) {
            patient_index <- patientIndex(record,
                                          record@patient[[id]])

            record@patients[[id]]
        }
        a<-record@patients[[id]]@episodes[[1]]
    }
}
