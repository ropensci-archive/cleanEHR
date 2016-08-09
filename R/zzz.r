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

    #' classification dictionary: demographic, nurse, physiology, laboratory, drugs
    class.dict_code <-  sapply(ITEM_REF, function(x) x$Classification1)
    class.dict_stname <- class.dict_code
    names(class.dict_stname) <- as.character(code2stname.dict)

    #' Time variable dictionary
    tval.dict_code <- data.checklist$NHICdtCode != "NULL" 
    tval.dict_stname <- tval.dict_code
    names(tval.dict_stname) <- as.character(data.checklist$NHICcode)
    names(tval.dict_code) <- as.character(data.checklist$NHICcode)

    ccdata.env$ITEM_REF <- ITEM_REF
    ccdata.env$code2stname.dict <- code2stname.dict
    ccdata.env$stname2code.dict <- stname2code.dict
    ccdata.env$class.dict_code <- class.dict_code
    ccdata.env$class.dict_stname <- class.dict_stname 
    ccdata.env$tval.dict_code <- tval.dict_code 
    ccdata.env$tval.dict_stname <- tval.dict_stname

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
