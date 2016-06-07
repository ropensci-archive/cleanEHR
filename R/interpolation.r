#' perform window period approximation on a given numeric vector. Window is
#' specified by lead and lag.
#' @param v vector
#' @param lead number of forward element from the centre of the window.
#' @export interpolateVec
interpolateVec <- function(v, lead, lag, FUN="mean", ...) {
    v <- suppressWarnings(as.numeric(as.character(v)))
    na.ind <- which(is.na(v))
    if (length(na.ind) > 0) { # do interpolation if NA is found. 
        v2 <- c(rep(NA, lead), v, rep(NA, lag))
        n_x <- sapply(na.ind, 
                      function(i) {
                          do.call(FUN,
                                  c(list(x=v2[i + lead + seq(-lead, lag)]),
                                    as.list(substitute(list(...)))[-1L]))
                      })
        v[na.ind] <- n_x
    }
    v
}

#' interpolate list format 
#' @param lead 
#' @param lag 
#' @param FUN function 
#' @export list_interpolation 
list_interpolation <- function(l, item_id, lead=1, lag=1, FUN=mean, ...) {
    if (!all(item_id %in% names(l[[1]])))
        stop('item_id cannot be found in the list.')

    check_ <- function(var, name) {
        if(length(item_id) != length(var)) {
            if (length(var) == 1) 
                var <- rep(var, length(item_id))
            else  stop(paste('length of', name, 'is not correct.', name, '=',
                             length(var), 'item_id = ', length(item_id)))
        }
        var
    }

    lean <- check_(lead, "lead")
    lag <- check_(lag, "lag")
    FUN <- check_(FUN, "FUN")

    lapply(l, 
           function(episode) {
               for (i in seq(item_id)) {
                   episode[[item_id[i]]] <-
                       interpolateVec(episode[[item_id[i]]], lead[[i]],
                                      lag[[i]], FUN[[i]], ...)
               }
               episode
           })
}
