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
    id <- removeIdPrefix(names(patient_unlist))
    ptable <- data.frame(id, val=as.vector(patient_unlist))
    
    valindex <- selectIndex(category.index.table, id, "timevars")
    stampindex <- selectIndex(category.index.table, id, "timestamp")


    if (length(valindex) != length(stampindex))# data missing values
        return(list(data1d=NULL, data2d=NULL))
    else
        data2d <- data.frame(id=ptable$id[valindex],
                         time=ptable$val[stampindex],
                         val=ptable$val[valindex])
    data2d <- table2array(data2d)
    data1d <- data.frame(id=ptable$id[selectIndex(category.index.table, id,
                                                  "simplevar")],
                         val=ptable$val[selectIndex(category.index.table, id,
                                                    "simplevar")])
    return(list(data1d=data1d, data2d=data2d))
}


#' convert xml data to ccdata format
#' @param xml xml root
#' @return ccdata  
xml2Ccdata <- function (xml, select.patient=NULL){
    patient.num <- xmlSize(xml[[1]][[2]])
    data2d <- list()
    data1d <- list()

    if(is.null(select.patient))
        select.patient <- seq(patient.num)

    category.index.table <- extractIndexTable() 

    pb <- txtProgressBar(min = min(select.patient)-1, 
                         max = max(select.patient), style = 3)

    for(patient.id in select.patient){
        patient <- getXmlPatient(xml, patient.id)
        pdata<- tryCatch(patientToDataArray(patient, category.index.table), 
                 error=function(err) {
                     cat(paste(err, "patient.id = ", patient.id, "\n"))
                     stop()
                 })
        if (!is.null(pdata[["data2d"]])) {
            data2d[[patient.id]] <- pdata[["data2d"]]
            data1d[[patient.id]] <- pdata[["data1d"]]
        }
        setTxtProgressBar(pb, patient.id)
    }
    cat("\n")
    return(list(data2d=data2d, data1d=data1d))
}



table2array <- function(data2d) {
    row.name <- sort(unique(data2d$time))
    col.name <- sort(unique(data2d$id))
    
    data.array <- array("", c(length(row.name), length(col.name)))

    rownames(data.array) <- row.name
    colnames(data.array) <- col.name

    for(i in seq(dim(data2d)[1]))
        data.array[as.character(data2d$time[i]), 
                   as.character(data2d$id[i])] <- data2d$val[i]
    return(data.array)
}
