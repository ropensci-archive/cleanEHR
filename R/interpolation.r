#' perform window period approximation on a given numeric vector. Window is
#' specified by leap and lag.
#' @param v vector
#' @param leap number of forward element from the centre of the window.
#' @export interpolateVec
interpolateVec <- function(v, leap, lag, FUN=mean) {
    v <- suppressWarnings(as.numeric(as.character(v)))
    na.ind <- which(is.na(v))
    v2 <- c(rep(NA, leap), v, rep(NA, lag))

    n_x <- sapply(na.ind, 
                  function(i) {
                      FUN(v2[i + leap + seq(-leap, lag)], na.rm=T)
                  })
    v[na.ind] <- n_x
    v
}

#' @export list_interpolation 
list_interpolation <- function(l, item_id, leap=1, lag=1) {
    lapply(l, 
           function(episode) {
               #print(episode[item_id])
               episode[[item_id]] <- interpolateVec(episode[[item_id]], leap, lag)
               episode
           })
}

getmesomevector<-function(x) {
    v=seq(x)
    v[round(runif(x/4)*x)]=NA
    v
}


