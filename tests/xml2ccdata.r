library("XML")

xmlLoad <- function(file) {
    file.parse <- xmlParse(file)
    xml.root <- xmlRoot(file.parse)
    return(xml.root)
}

getPatient <- function(xml.root, id) {
    xml.root[[1]][[2]][[id]]
}

xml2Ccdata <- function (xml, data.checklist){

    patient.num <- xmlSize(xml[[1]][[2]])
    data2d <- list()
    data1d <- list()
    pb <- txtProgressBar(min = 1, max = patient.num, style = 3)
    
    for(patient.id in seq(patient.num)){
        patient <- getPatient(xml, patient.id)
        data <- patientToDataArray(patient, info.list$time, info.list$meta)
        data2d[[patient.id]] <- data$data2d
        data1d[[patient.id]] <- data$data1d
        setTxtProgressBar(pb, patient.id)
    }
}


patientToTable <- function(patient){
    p.table <- unlist(xmlToList(patient, addAttributes=FALSE))
    id <- unlist(strsplit(
                   as.vector(names(p.table)), 
                   "NIHR_HIC_ICU_0"))[seq(2, length(p.table)*2, 2)]
    return(data.frame(id, val=as.vector(p.table)))
}

# TODO: needs more explaination here, parce que c'est le bordel ici!
# the algorithm may have a flaw too. Double check!!!
patientToDataArray <- function(patient, time.list, meta.list){
    pt <- patientToTable(patient)
    # pick out time id from normal ids 
    # timeid(id), itemid(idt)
    this.time.list <- time.list[intersectIndexB(pt$id, time.list$idt), ]
    time.index <- intersectIndexB(this.time.list$idt, pt$id)
    item.index <- intersectIndexB(this.time.list$id, pt$id)
    pt_ <- data.frame(time=pt$val[time.index], 
                             item=pt$val[item.index],
                             ids=pt$id[item.index])

    # stupid algorithm
    rowname <- sort(unique(pt_$time))
    colname <- sort(unique(pt_$id))
    data.array <- array("", c(length(rowname), length(colname)))
    rownames(data.array) <- rowname
    colnames(data.array) <- colname
    for(i in seq(pt_$id)){
        col.ind <- which(colname==pt_$id[i])
        row.ind <- which(rowname==pt_$time[i])
        data.array[row.ind, col.ind] <- as.character(pt_$item[i])
    }

    data1d <- as.character(pt$val[!(time.index | item.index)])
    names(data1d) <- as.character(pt$id[!(time.index | item.index)])
    
    return(list(data2d=data.array, data1d=data1d))
}

#'@description provides the intersection index coresponding to \code{b}
#'@param a vector
#'@param b vector
#'@return vector contains [TRUE, FALSE], as the same lenght as \code{b} 
intersectIndexB <- function(a, b) {
    inct <- intersect(a, b)
    return(b %in% inct)
}


.extractInfo <- function(checklist) {
    index.time <- grepl(checklist$NHICdtCode, pattern="0[0-9][0-9][0-9]")
    index.meta <- grepl(checklist$NHICmetaCode, pattern="0[0-9][0-9][0-9]")
    time.list <- data.frame(id=.removeIdPrefix(checklist$NHICcode[index.time]), 
                        idt=.removeIdPrefix(checklist$NHICdtCode[index.time]))
    meta.list <- data.frame(id=.removeIdPrefix(checklist$NHICcode[index.meta]), 
                        idmeta=.removeIdPrefix(checklist$NHICmetaCode[index.meta]))

    return(list(time=time.list, meta=meta.list))
}

.removeIdPrefix <- function(id) {
    no.prefix <- unlist(strsplit(as.character(id), 
                    "NIHR_HIC_ICU_0"))[seq(2, length(id) * 2, 2)]
    if(any(is.na(as.numeric(no.prefix))))
       stop("id should be like NIHR_HIC_ICU_0xxx\n")
    return(no.prefix)
}


r <- xmlLoad("../CriticalCare/test.xml")
p1 <- getPatient(r, 1)
data.checklist <- read.csv("data/data_item.csv")
data <-xml2Ccdata(r, data.checklist)
save("xmlData.Rdata", data)
