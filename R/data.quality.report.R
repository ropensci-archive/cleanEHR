#' Create the data quality report
#' 
#' @export data.quality.report
#' @import knitr
data.quality.report <- function(ccd, pdf=T) {
    
    if (dir.exists(".report")) {
        unlink(".report", recursive=T)
    }

    dir.create(".report")
    wd <- getwd()
    rptpath <- paste(path.package('ccdata'), "report", sep="/")
    file.copy(rptpath, ".report", recursive=T)

    setwd('.report/report')
    dqpath <- "data_quality_report.Rmd"
    headerpath <- "listings-setup.tex"
    tpltpath <- "report.latex"

    knit(dqpath, "data_quality_report.md")
    if (pdf) {
        pandoc.cmd <- 
            paste("pandoc -s -N --toc --listings -H ", headerpath,
                  " --template=", tpltpath, 
                  " -V --number-section  -V papersize:a4paper -V geometry:margin=1.3in ", 
                  "data_quality_report.md -o data_quality_report.pdf", sep="")
        tryCatch(system(pandoc.cmd), 
                 error = function(e) {
                     cat(e)
                     setwd(wd)
                 }, 
                 finally = {
                     setwd(wd)
                 })
        setwd(wd)
    }
}


#' @export file.summary
file.summary <- function(ccd) {
    infotb <- ccd@infotb
    file.summary <- infotb[, list("Number of Episode"=.N, 
                                  "Upload time"=max(parse_time), 
                                  "Sites"=paste(unique(site_id), collapse=", ")), by=parse_file]
    file.summary[, "File":=parse_file]
    file.summary[, parse_file:=NULL]
    return(file.summary)
}

#' @export xml.file.duration.plot
xml.file.duration.plot <- function(ccd) {
    tb <- copy(ccd@infotb)
    tb <- tb[, list(minadm=min(t_admission, na.rm=T), 
              maxadm=max(t_admission, na.rm=T),
              mindis=min(t_discharge, na.rm=T),
              maxdis=max(t_discharge, na.rm=T)), by=parse_file]
    ggplot(tb, aes(x=minadm, y=parse_file)) +
        geom_segment(aes(xend=maxdis, yend=parse_file), color="gray", size=7) +
        annotate("text", x=tb$minadm+(tb$maxdis-tb$minadm)/2, 
                 y=tb$parse_file, label=tb$parse_file, size=3) + 
        scale_x_datetime(date_breaks="3 month")+
        theme(axis.text.y=element_blank()) + 
        ggtitle("The Duration of XML Files") +
        xlab("") + ylab("")
}


ethnicity.plot <- function(demg) {
    ggplot(demg, aes(x=ETHNIC, fill=ETHNIC)) + 
        scale_fill_discrete(h=c(50,250)) +
        geom_bar(aes(y=(..count..)/sum(..count..) * 100)) + 
        geom_text(aes(y = ((..count..)/sum(..count..))*100,
                      label = scales::percent((..count..)/sum(..count..))),
                  stat = "count", vjust = -0.25)
}



#demographic.data.completeness <- function(ccd) {
#    demographic
#}

#' 
#' @export total.data.point
total.data.point <- function(ccd) {
    dp.physio <- 
        sum(unlist(for_each_episode(ccd, 
                                    function(x) 
                                        Reduce(sum, sapply(x@data, nrow)))))
    dp.demg <-
        sum(unlist(for_each_episode(ccd, 
                                    function(x) 
                                        Reduce(sum, sapply(x@data, nrow)))))
    return(sum(dp.physio, dp.demg))
}

#' @export table1.row
table1.row <- function(demg, name) {
    ref <- ccdata:::ITEM_REF[[stname2code(name)]]
    if (ref$Datatype %in% c("text", "list")) {
        stopifnot(!is.null(ref$category))
        nmref <- sapply(ref$category$levels, function(x) x)
        r <- demg[, .N, by=name]
        level.name <- nmref[r[[name]]]
        r[, nm:=level.name]
        r[, percent:=N/nrow(demg)*100]

        txt <- paste("<li>",
                     r$nm, ":\t\t\t\t", 
                     r$N, "[", 
                     round(r$percent),"%]",
                     "</li>",
                     sep="", 
                     collapse="")
        txt <- paste("<ul>", txt, "<ul>")
    }
    return(txt)



}
