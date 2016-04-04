#' Compute the score for single patient
#' @param items.code an array of items code to be concerned.
#' @param range list
#' @return score
computePatientScore <- function(data.set, patient.id, 
                                items.code, ranges,
                                score.ref, fun.list) {
    final.score <- 0
    for(i in seq(items.code)){
        final.score <- final.score + 
            computeItemScore(data.set, patient.id, 
                             items.code[[i]], ranges[[i]],
                             score.ref[[i]], fun.list[[i]])
    }
    return(final.score)
}

#' compute one item of the socre of a single patient.
#' @param data.set
#' @param patient.id
#' @return score in integer.
computeItemScore <- function(data.set, patient.id, 
                             item.code, range, 
                             score.ref, fun) {
    data <- getPatient2dItem(data.set$data2d, item.code, patient.id)
    value <- fun(data$time, data$val)
    return(score.ref[findInterval(value, range, all.inside=TRUE)])
}

#' compute scores of a given score system to multiple/all patients of a data
#' set.
#' @param data.set a list which contains data2d and data1d
#' @param items.code ... 
computeScore <- function(data.set, items.code, ranges, 
                         score.ref, fun.list, check.items.code, patient.id=NULL) {
    if (is.null(patient.id))
        patient.id <- seq(length(data.set$data2d))
    score.list <- list()
    for(id in patient.id) {
        score.list[[id]] <- list()
        score <- computePatientScore(data.set, id, 
                                     items.code, ranges, 
                                     score.ref, fun.list)
        val1d <- as.character(getPatient1dItem(data.set$data1d, 
                                               check.items.code,
                                               patient.id=id))
        if (length(val1d) != 0 & !is.null(score)) {
            score.list[[id]][["score"]] <- score
            score.list[[id]][["items"]] <- data.frame(item=check.items.code,
                                                      val=val1d)
        }
    }
    return(score.list)
}
