library(ccdata)
if (!exists("ccd_delta_num"))
    load("../data/delta_num.Rdata")

is.2d <- function(codes) {
    c2d <- vector()
    for (i in codes) {
    if(getItemInfo(i)['dt_code'] != "NULL")
        c2d <- c(c2d, i)
    }
    c2d
}


longname <- function(x) {
    ln <- vector()
    for(i in x)
        ln <- c(ln, ccdata.env$ITEM_REF[[i]]$dataItem)
    return(ln)
}


yml <- yaml.load_file('../paper-brc/data/ANALYSIS_REF.yaml')

var2d <- is.2d(names(yml))
labels <- longname(var2d)


tb <- selectTable(ccd_delta_num, items_opt=var2d, freq=1, item.name=labels)
