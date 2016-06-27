#' @include ccTable.R


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



ccTable$methods(
    get.ranges = function() {

        # Initialise with temp column to make sure that dquality has the same
        # number of rows than torigin 
        .self$dquality$range <- .self$torigin[,c('site', 'episode_id'), with=F]
        rgnum <- list('red'=1, 'amber'=2, 'green'=3)
        for(item_name in names(.self$conf)) {
            item <- .self$conf[[item_name]]

            if (!is.null(item$range$labels)){
                rgclass <- rep(NA, nrow(.self$torigin))
                rgclass[.self$torigin[[item_name]] != "NA"] <- 0
                                
                for(rg_label in names(item$range$labels)) {
                    irg <- item$range$labels[[rg_label]]
                    rgclass[which(inrange(.self$torigin[[item_name]], irg))] <- 
                        rgnum[[rg_label]]
                }
                .self$dquality$range[[item_name]] <- rgclass
            }
        }
    }
)




ccTable$methods(
    filter.ranges = function(select='red') {
        rgnum <- list('red'=1, 'amber'=2, 'green'=3)
        inselectrange <- function(x, ...) {
            x > rgnum[[select]]
        }

        if(is.null(.self$dquality$range) || 
           nrow(.self$dquality$range) != nrow(.self$tclean))
            .self$get.ranges()

        .self$dfilter$range <- getfilter(.self$dquality$range, inselectrange)
    }
)
