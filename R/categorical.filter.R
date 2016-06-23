#' @include ccTable.R

#' @export item.criterion
item.criterion <- function(conf, criterion) {
    names(tt$conf)[sapply(tt$conf, function(x) criterion %in% names(x))]
}

ccTable$methods(
    get.categories = function() {
        citems <- item.criterion(conf, "category")
        lapply(.self$conf[citems], function(x) names(x$category))
    })

ccTable$methods(
    get.data.column = function(filter) {
        citems <- item.criterion(.self$conf, filter)
        return(.self$torigin[, c("site", "episode_id", citems), with=FALSE])
    }
)

ccTable$methods(
    filter.category = function() {
        "Check individual entries if they are the in the categories specified
        in conf."
        data <- .self$get.data.column("category")
        categories <- get.categories()
        in.category <- function(x, name) 
            x %in% c(categories[[name]], "NA", NA)
        .self$dfilter$category <- getfilter(data, in.category)
    })
