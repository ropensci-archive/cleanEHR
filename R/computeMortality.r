#' compute mortality rate in each score from a data.frame which contains scores
#' and mortality.
#' @param score.mort
#' @return score, mortality rate.
computeMortalityRate <- function(score.mort) {
    score <- sort(unique(score.mort$score))
    death.rate <- vector()
    for(s in seq(score)) {
        mortality.this.score<- score.mort$mortality[score.mort$score==score[s]]
        live.rate <-
            length(which(mortality.this.score=="A"))/length(mortality.this.score)
        death.rate[s] <- 1-live.rate
    }
    return(data.frame(score, death.rate))
}
