.onLoad <- function(libname = find.package("ccdata"), pkgname = "ccdata") {
    ccdata.env <<- new.env()
    path <- find.package("ccdata")
    
    data("data.checklist", package="ccdata")
    ccdata.env$ITEM_REF <- yaml.load_file(paste(path, "data", "ITEM_REF.yaml", sep=.Platform$file.sep))
    
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
