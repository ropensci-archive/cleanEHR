
# Spiros Denaxas
# UCL Institute of Health Informatics
#
# Set of functions to validate ICNARC
# terms using a prebuilt tree.

library(data.tree)

if (!exists("icnarcTree")) {
    icnarcTree <- readRDS("data/icnarc.rds")
}

#' Check if input is a valid ICNARC code
#'
#' @param x Input string
#' @return TRUE if valid, FALSE is not
#' @examples
#' is.valid.icnarc.code('1.1')
#' is.valid.icnarc.code('2.1.4.27.5')
#' 

is.valid.icnarc.code <- function( input ){
    
    if ( ! is.na( input ) ){
        if ( ! is.null( Navigate(icnarcTree, gsub('\\.', '/', input) ) ) ){
            return(TRUE)
        }
    }
    return(FALSE)
}

#' Normalize an ICANRC code
#'
#' @param x Input string
#' @return string with nornalized code, FALSE on error
#' @examples
#' normalize.icnarc.code ('1.01.02') returns 1.1.2
#' 

normalize.icnarc.code <- function( input ){
    if ( is.na( input ) ){
        return(FALSE)
    }
    
    splitCode      <- strsplit(input, '\\.')
    part_surgical  <- as.integer( splitCode[[1]][1] )
    part_system    <- as.integer( splitCode[[1]][2] )
    part_site      <- as.integer( splitCode[[1]][3] )
    part_process   <- as.integer( splitCode[[1]][4] )
    part_condition <- as.integer( splitCode[[1]][5] )
    
    output <- c( 
        na.omit(part_surgical), 
        na.omit(part_system), 
        na.omit(part_site),
        na.omit(part_process), 
        na.omit(part_condition) )
    
    return( paste(output, collapse = ".") )
}