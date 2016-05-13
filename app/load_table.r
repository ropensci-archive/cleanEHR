library(ccdata)
if (!exists("ccd_delta_num"))
    load("../data/delta_num.Rdata")

# find out 2D data labels
is.2d <- function(codes) {
    c2d <- vector()
    for (i in codes) {
    if(getItemInfo(i)['dt_code'] != "NULL")
        c2d <- c(c2d, i)
    }
    c2d
}

# get long names of items
longname <- function(x) {
    ln <- vector()
    for(i in x)
        ln <- c(ln, ccdata.env$ITEM_REF[[i]]$dataItem)
    return(ln)
}


yml <- yaml.load_file('../paper-brc/data/ANALYSIS_REF.yaml')

var2d <- is.2d(names(yml))
labels <- longname(var2d)


tb <- selectTable(ccd_delta_num, items_opt=var2d, freq=1, item.name=labels,
                  return_list=TRUE)
lt <- list_interpolation(tb, "NHIR_HIC_ICU_0108", lead=5, lag=5)
