#' @include ccTable.R

ccTable$methods(
    imputation = function() {
        imputation_columns <- function(sd) {
            for (i in names(.self$conf)) {
                imwin <- .self$conf[[i]][['missingness_2d']][['impute_2d']]
                if (!is.null(imwin)) {
                    fun <- imwin[['fun']]
                    lead <- imwin[['lead']]
                    lag <- imwin[['lag']]
                    sd[[i]] <- interpolateVec(v=sd[[i]], lead=lead, lag=lag, FUN=fun, na.rm=T)
                }
            }
            return(sd)
        }
        .self$tclean <- .self$tclean[, imputation_columns(.SD), by=c("episode_id", "site")]
})
