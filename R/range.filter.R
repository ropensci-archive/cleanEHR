#' @include ccDataTable2.R


# Check if the values of a vector v is in the ranges.
# @param v vector numeric
# @param range characters numeric ranges in a form such as (low, up). Multiple
#       ranges should seperated by semi-column.
inrange <- function(v, range) {
    funtxt <- function(r) {
        r <- gsub(";", "|", r)
        r <- gsub(",", " < v & v < ", r)
        return(paste("function(v)", r))
    }

    cmpfunc <- eval(parse(text=funtxt(range)))
    return(cmpfunc(v))
}

ccDataTable$methods(
    filter.ranges = function(select='red') {
        rgnum <- list('red'=1, 'amber'=2, 'green'=3)
        if(is.null(.self$data_quality$range) || nrow(.self$data_quality$range) != nrow(.self$tclean))
            .self$get.ranges()
        for(item in names(.self$data_quality$range)) 
            .self$tclean[[item]][.self$data_quality$range[[item]] < rgnum[[select]]] <- NA 
    }
)

ccDataTable$methods(
    get.ranges = function(){
        if (is.null(.self$data_quality$range))
            .self$data_quality$range <- data.table(seq(nrow(.self$torigin)))#.self$torigin[,c('site', 'episode_id'), with=F]
        rgnum <- list('red'=1, 'amber'=2, 'green'=3)
        for(item_name in names(.self$conf)) {
            item <- .self$conf[[item_name]]

            if (!is.null(item[['range']])){
                rgclass <- rep(NA, nrow(.self$torigin))
                rgclass[.self$torigin[[item_name]] != "NA"] <- 0
                                
                for(rg_label in names(item[['range']])) {
                    irg <- item[['range']][[rg_label]]
                    rgclass[which(inrange(.self$torigin[[item_name]], irg))] <- 
                        rgnum[[rg_label]]
                }
                .self$data_quality$range[[item_name]] <- rgclass
            }
        }
    }
)


