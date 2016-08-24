#' @include ccEpisode.R
NULL

#' ccRecord is a class to hold the raw episode data parsed directly from XML or
#' CSV files.
#' @slot nepisodes an integer number describe the number of episode the record
#'       held.
#' @slot dmgc a data.table containing all the demographic information of each
#'       episode.
#' @slot parseinfo a data.table holding the parsing information of each episode such as the
#'       parsing time and from which file it parsed from.
#' @exportClass ccRecord2
ccRecord2 <- setClass("ccRecord2", 
                      slots=c(nepisodes="integer", dmgc="data.table", 
                              parseinfo="data.table", episodes="list"),
                      prototype=prototype(nepisodes=as.integer(0)))
