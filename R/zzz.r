.onLoad <- function(libname = find.package("ccdata"), pkgname = "ccdata") {
    ccdata.env <<- new.env()
    assign('code_pas_number',  getItemInfo("PAS number")["NHIC_code"], envir=ccdata.env)
    assign('code_nhs_number',  getItemInfo("NHS number")["NHIC_code"], envir=ccdata.env)
    assign('code_episode_id', 
           getItemInfo("Critical care local identifier / ICNARC admission number")["NHIC_code"], 
           envir=ccdata.env)
    assign('code_site_id', 'NIHR_HIC_ICU_0002', envir=ccdata.env)
    assign('info.index', extractIndexTable(), envir=ccdata.env)
}
