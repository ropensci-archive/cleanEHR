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
