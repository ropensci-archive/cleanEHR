#' Check if the values of a vector v is in the given ranges.
#'
#' @param v vector numeric
#' @param range A string contains the numeric ranges in a form such as (low,
#' up) for open range and [low, up] for close range. Multiple
#' ranges should be separated by semi-columns which is equivalent to logical
#' OR e.g. (low1, up1); (low2, up2)
inrange <- function(v, range) {
    funtxt <- function(r) {
        spr <- unlist(strsplit(r, ";"))
        number <- regmatches(spr, gregexpr("[-+]?[0-9]*\\.?[0-9]+", spr))
        operation <- regmatches(spr, gregexpr("\\[|\\]|\\(|\\)", spr))
        
        if (any(is.na(as.numeric(unlist(number)))))
            stop("Cannot find the proper numeric range expression.")

        stopifnot(length(number) == length(operation))
        total.op <- vector()

        for (i in seq(operation)) {
            op <- operation[[i]]
            op[op == "("] <- "x >"
            op[op == "["] <- "x >="
            op[op == ")"] <- "x <"
            op[op == "]"] <- "x <="
            if (as.numeric(number[[i]][1]) > as.numeric(number[[i]][2]))
                stop("the lower bound value (", number[[i]][1], ') is bigger than the upper bound value (', number[[i]][2], ")")
            total.op[i] <- paste("(", paste(op, number[[i]], collapse=" & "), ")")
        }
        total.op <- paste(total.op, collapse=" | ")
        return(paste("function(x) ", total.op))
    }
    cmpfunc <- eval(parse(text=funtxt(range)))
    return(cmpfunc(v))
}


getfilter <- function(dq, criterion) {
            kn <- c("site", "episode_id")
            if (all(kn %in% names(dq)) & length(names(dq)) > 2) {
                keys <- dq[, kn, with=FALSE]
                k_ind <- !(names(dq) %in% kn)
                dq <- dq[, names(dq)[k_ind], with=FALSE]
                # updating range entry with true/false values
                dq <- dq[, Map(criterion, .SD, names(.SD))]
                # adding site and episode_id columns.
                entry <- data.table(keys, dq)
                episode <- entry[, 
                                 all(unlist(.SD), na.rm=TRUE), 
                                 by=c("site", "episode_id")]
                setnames(episode, c("site", "episode_id", "select_index"))
                return(list(entry=entry, episode=episode))
            }
            else return(NULL)
        }


#' @include ccTable.R
ccTable$methods(
    inspect_range = function() {
        # Initialise with temp column to make sure that dquality has the same
        # number of rows than torigin 
        .self$dquality$range <- .self$torigin[,c('site', 'episode_id'), with=FALSE]
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


#' Numerical range filter
#' 
#' Range filter can only be applied on numerical fields. 
#' For those fields which requires a range filter to be applied, 
#' one needs to set a series ranges from the broadest to the narrowest in
#' the YAML configuration. We can set three levels (labels) of ranges, red, amber, and
#' green.  It is also OK to set only one range instead of three. 
#' The range filter will first assign a label to every data entry. 
#' 
#' The range in the YAML configuration file can be (l, h), [l, h], (l, h], [h, l) 
#' standing for close, open and half open intervals. 
#' @param select the range label - "red", "amber", "green"
#' If I give "yellow to select, it means I only want the values which is
#' labeled as "yellow" to be in the clean table.
#' @name ccTable_filter_range
#' @examples 
#' \dontrun{
#' # YAML example
#' NIHR_HIC_ICU_0108:
#'    shortName: h_rate
#'    dataItem: Heart rate
#'    range:
#'     labels:
#'      red: (0, 300)     # broader
#'      amber: (11, 170) 
#'      green: (60, 100)  # narrower
#'     apply: drop_entry
#' # apply range filter on ccTable ct
#' ct$filter_range("yellow")
#' ct$apply_filters
#'}
ccTable$methods(
    filter_range = function(select='red') {
        rgnum <- list('red'=1, 'amber'=2, 'green'=3)
        # dq can be either dqaulity table or torigin
        # criterion should be a function to give T/F values of each entry.
        
        inselectrange <- function(x, ...) {
            x >= rgnum[[select]]
        }

        if(is.null(.self$dquality$range) || 
           nrow(.self$dquality$range) != nrow(.self$tclean))
            .self$inspect_range()

        .self$dfilter$range <- getfilter(.self$dquality$range, 
                                         inselectrange)
    }
)
