#' compute mortality rate in each score from a data.frame which contains scores
#' and mortality.
#' @param score.mort
#' @return score, mortality rate.
computeMortalityRate <- function(score.mort) {
    score <- sort(unique(score.mort$score))
    death.rate <- vector()
    death.num <- vector()
    for(s in seq(score)) {
        mortality.this.score<- score.mort$mortality[score.mort$score==score[s]]
        total.num <- length(mortality.this.score)
        alive.num <- length(which(mortality.this.score=="A"))
        death.rate[s] <- 1 - alive.num/total.num
        death.num[s] <- total.num - alive.num
    }
    return(data.frame(score, death.rate, death.num))
}
