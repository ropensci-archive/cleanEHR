#' Fill NA values of its first non-NA previous value.
#' @export fill.na 
fill.na= function(x) {
    ind = which(!is.na(x))
    if(is.na(x[1]))
          ind = c(1,ind)
    rep(x[ind], times = diff(c(ind, length(x) + 1) )) 
}
