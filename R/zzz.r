.onLoad <- function(libname = find.package("ccdata"), pkgname = "ccdata") {
    ccdata.env <<- new.env()
    
    reverse.name.value <- function(vec) {
        new <- names(vec)
        names(new) <- vec
        return(new)
    }
    path <- find.package("ccdata")
    data("data.checklist", package="ccdata")
    
    ITEM_REF <- yaml.load_file(paste(path, "data", "ITEM_REF.yaml", sep=.Platform$file.sep))
    code2stname.dict <- sapply(ITEM_REF, function(x) x$shortName)
    stname2code.dict <- reverse.name.value(code2stname.dict)

    ccdata.env$ITEM_REF <- ITEM_REF
    ccdata.env$code2stname.dict <- code2stname.dict
    ccdata.env$stname2code.dict <- stname2code.dict

    
    assign('code_pas_number',  getItemInfo("PAS number")["NHIC_code"], envir=ccdata.env)
    assign('code_nhs_number',  getItemInfo("NHS number")["NHIC_code"], envir=ccdata.env)
    assign('code_episode_id', 
           getItemInfo("Critical care local identifier / ICNARC admission number")["NHIC_code"], 
           envir=ccdata.env)
    assign('code_site_id', 'NIHR_HIC_ICU_0002', envir=ccdata.env)
    assign('code_deadicu_id', 
           getItemInfo("Dead or alive on discharge")["NHIC_code"], envir=ccdata.env)
    assign('code_admin_icu_t', 
           getItemInfo("Date & Time of admission to your unit")["NHIC_code"],
           envir=ccdata.env)
    assign('code_discharge_icu_t',
           getItemInfo("Date & Time of discharge from your unit")["NHIC_code"],
           envir=ccdata.env)

    assign('checklist', extractIndexTable(), envir=ccdata.env)

}

#' This is a patch for reloading environment variables after doing clean all.
#' @export recreate.env
recreate.env <- function() {
    .onLoad()
}
