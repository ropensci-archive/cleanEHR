#' this is ref example
#' @include ccdata.r
#' @export ccDataTable2
ccDataTable2 <- setRefClass("ccDataTable2", 
                            fields=c(conf="list",
                                     origin_table="data.table", 
                                     clean_table="data.table",
                                     record="ccRecord", 
                                     missingness_table="data.table",
                                     range_table="data.table"))

ccDataTable2$methods(
show = function() {
    cat("origin_table:\n------\n")
    print(.self$origin_table)
    cat("clean_table:\n------\n")
    print(.self$clean_table)
})

ccDataTable2$methods(
create.table = function(freq){
    "Create a table contains the selected items in the conf with a given
    frequency (in hour)"
    items <- names(.self$conf)
    .self$origin_table <- selectTable(record=record, items_opt=items, 
                               freq=freq)
    .self$clean_table <- .self$origin_table
})

ccDataTable2$methods(
filter.missingness = function(apply=TRUE){
    "filter out the "
})



ccDataTable2$methods(
filter.null = function(items=c("episode_id", "site")) {
    "remove the entire episode when the episode_id or site is NULL"
    for (i in items)
        .self$clean_table <- .self.clean_table[i != "NULL"]
})

ccDataTable2$methods(
reload.conf = function(file) {
    "reload yaml configuration."
    .self$conf=yaml.load_file(file)
})

