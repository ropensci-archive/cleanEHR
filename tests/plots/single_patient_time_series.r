library(dataplay)
library(devtools)
library(grid)
library(gridExtra)
reload(".")


if(!exists("ccd"))
    load("xmlData.Rdata")


plotPatientItem <- function (data.set, patient.id, item, demographic=NULL) {
    item.info <- getItemInfo(item)
    item.data <- getPatient2dItem(data.set$data2d, item.info['NHIC_code'],
                                  patient.id)
    item.data$val <- as.numeric(as.character(item.data$val))
    print(item.data)
    item.data <- item.data[!is.na(item.data$val), ]

    gg <- ggplot(item.data, aes(time,val))
    gg <- gg + geom_point() + geom_line()
    gg <- gg + scale_x_datetime(breaks="24 hours")
    ylabel <- ""
    if (!is.null(item.info['unit']) & item.info['unit'] != "NULL")
        ylabel <- paste(ylabel, as.character(item.info['unit']), sep="")
    if (!is.null(item.info['meta_code']) & item.info['meta_code'] != "NULL")
        ylabel <- paste(ylabel, as.character(item.info['meta_code']), sep="")
    gg <- gg + ylab(ylabel)
    gg <- gg + ggtitle(item.info['item'])
    return(gg)
}


p1 <- plotPatientItem(ccd, 1000, "SpO2")
p2 <- plotPatientItem(ccd, 1000, "Heart rate")
grid.arrange(p1, p2, ncol=2)
