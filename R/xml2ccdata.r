library("XML")

#' load xml clinical data
#' @return the root of the xml data
xmlLoad <- function(file) {
    file.parse <- xmlParse(file)
    xml.root <- xmlRoot(file.parse)
    return(xml.root)
}

#' get the patient data from xml 
#' @param xml.root root of xml data returned by xmlLoad()
#' @return patient xml data
getXmlPatient <- function(xml.root, id) {
    xml.root[[1]][[2]][[id]]
}

#' convert xml data for a given patient and convert them into a list
#' @param patient individual xml patient data
#' @param time.list
#' @param meta.list
#' @return list
patientToDataArray <- function(patient, category.index.table) {
    patient_unlist <- unlist(xmlToList(patient, addAttributes=FALSE))
    stdid <- StdId(names(patient_unlist))
    ptable <- data.frame(id=stdid@ids, val=as.vector(patient_unlist))

    valindex <- selectIndex(category.index.table, stdid, "timevars")
    stampindex <- selectIndex(category.index.table, stdid, "timestamp")

    if (length(valindex) != length(stampindex))# data missing values
        return(NULL)
    else
        data2d <- data.frame(id=ptable$id[valindex],
                             time=ptable$val[stampindex],
                             val=ptable$val[valindex],
                             stringsAsFactors=FALSE)
    data1d <- 
        data.frame(id=ptable$id[selectIndex(category.index.table, 
                                            stdid,"simplevar")],
                   val=ptable$val[selectIndex(category.index.table, 
                                              stdid, "simplevar")],
                   stringsAsFactors=FALSE)
    return(list(data1d=data1d, data2d=data2d))
}


#' convert xml data to ccdata format
#' @param xml xml root
#' @return ccdata  
xml2Data <- function (xml, select.patient=NULL, quiet=TRUE){
    patient.num <- xmlSize(xml[[1]][[2]])
    if(is.null(select.patient))
        select.patient <- seq(patient.num)

    category.index.table <- extractIndexTable() 

    if (!quiet)
        pb <- txtProgressBar(min = min(select.patient)-1, 
                             max = max(select.patient), style = 3)
    record <- ccRecord()

    for(patient.id in select.patient){
        episode_i <- ccEpisode()
        patient <- getXmlPatient(xml, patient.id)
        pdata<- tryCatch(patientToDataArray(patient, category.index.table), 
                         error=function(err) {
                             cat(paste(err, "patient.id = ", patient.id, "\n"))
                             stop()
                         })

        if (!is.null(pdata)) {
            if (nrow(pdata[["data1d"]]) != 0)
                episode_i <- episode_i + pdata[["data1d"]]
            if (nrow(pdata[["data2d"]]) != 0)
                episode_i <- episode_i + pdata[["data2d"]]
        }

        record <- record + episode_i
        if (!quiet)
            setTxtProgressBar(pb, patient.id)
    }
    if (!quiet)
        cat("\n")

    setkey(record@nhs_numbers, nhs_number)
    setkey(record@pas_numbers, pas_number)

    return(record)
}
