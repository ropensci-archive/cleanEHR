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

#' Categorical data filter
#'
#' Categorical variables only allow a set of values to appear in the variable
#' . Due to various reasons, a categorical variable may contain values that are not 
#' standard. The allowed values can be set in the YAML configuration while initilising 
#' the ccTable (see ccTable-class, create_cctable).  
#' In the following example, we can see how to set up the categorical filter 
#' for the variable dead_icu (NIHR_HIC_ICU_0097) which only allows its value to 
#' be A, D, E.
#' 
#' @name ccTable_filter_categories
#' @examples
#' \dontrun{
#' # Example for categorical filter setup in the YAML configuration
#' NIHR_HIC_ICU_0097:
#'  category:
#'    levels:
#'      A: Alive
#'      D: Dead
#'      E: Alive - not discharged
#'    apply: drop_entry
#' 
#' # Run the filter on ccTable ct
#' ct$filter_categories() # run the filter
#' ct$apply_filters()     # apply the filter and create the clean table
#' }
NULL
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
