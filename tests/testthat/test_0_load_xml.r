# load test data
#if (!exists("ccd")) {
    #ccd <<- xml2Data("../data/test_data_10_patients.xml", seq(3), quiet=TRUE)
    # remove the first null patient
    #ccd_ <<- ccRecord()
    #ccd_ <<- ccd_ + ccd[2, 1] + ccd[3, 1]

    load("../../inst/doc/sample_ccd.RData")
    tb <<- create_cctable(ccd, "../data/ANALYSIS_REF.yaml", 1)
    rm("ccd")
    ccd <<- xml2Data("../data/test_data_anonym.xml")
    ccdt <<- deltaTime(ccd, pseudotime=TRUE)
#}
