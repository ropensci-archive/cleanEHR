#' @include ccTable.R

item.criterion <- function(conf, criterion) {
    names(conf)[vapply(conf, 
                       function(x) criterion %in% names(x), 
                       logical(1))]
}

ccTable$methods(
    inspect_categories = function() {
        citems <- item.criterion(conf, "category")
        lapply(.self$conf[citems], function(x) names(x$category$levels))
    })

ccTable$methods(
    get.data.column = function(filter) {
        citems <- item.criterion(.self$conf, filter)
        return(.self$torigin[, c("site", "episode_id", citems), with=FALSE])
    }
)

ccTable$methods(
    filter_categories = function() {
        "Check individual entries if they are the in the categories specified
        in conf."
        data <- .self$get.data.column("category")
        categories <- inspect_categories()
        in.category <- function(x, name) 
            x %in% c(categories[[name]], "NA", NA)
        .self$dfilter$category <- getfilter(data, in.category)
    })
