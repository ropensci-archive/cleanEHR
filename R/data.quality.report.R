#' Create the data quality report
#' 
#' @export data.quality.report
#' @import knitr
data.quality.report <- function(ccd, pdf=T) {
    
    if (!dir.exists(".temp")) {
        unlink(".temp", recursive=T)
        dir.create(".temp")
    }
    wd <- getwd()
    rptpath <- paste(path.package('ccdata'), "report", sep="/")
    file.copy(rptpath, ".temp", recursive=T)

    setwd('.temp/report')
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
