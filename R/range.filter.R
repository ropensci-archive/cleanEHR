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

            if (!is.null(item[['range']])){
                rgclass <- rep(NA, nrow(.self$torigin))
                rgclass[.self$torigin[[item_name]] != "NA"] <- 0
                                
                for(rg_label in names(item[['range']])) {
                    irg <- item[['range']][[rg_label]]
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
        inselectrange <- function(x) {
            x < rgnum[[select]]
        }
        allinselectrange <- function(x) {
            all(x < rgnum[[select]], na.rm=TRUE)
        }

        rgnum <- list('red'=1, 'amber'=2, 'green'=3)
        if(is.null(.self$dquality$range) || nrow(.self$dquality$range) != nrow(.self$tclean))
            .self$get.ranges()

        # temporarily remove site episode_id column
        temp <- .self$dquality$range[, c(-1, -2), with=FALSE]
        .self$dfilter$range <- list()
        # updating range entry with true/false values
        temp <- temp[, lapply(.SD, inselectrange)]
        # adding site and episode_id columns.
        .self$dfilter$range$entry <- 
            data.table(.self$dquality$range[, c("site", "episode_id"), with=FALSE], temp)
        .self$dfilter$range$episode <- 
            .self$dfilter$range$entry[, allinselectrange(unlist(.SD)), 
                                      by=c("site", "episode_id")]
        setnames(.self$dfilter$range$episode, c("site", "episode_id",
                                                "select_index"))
    }
)
