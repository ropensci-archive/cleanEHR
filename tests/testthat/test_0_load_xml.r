# load test data
#if (!exists("ccd")) {
    ccd <<- xml2Data("../data/test_data_10_patients.xml", seq(3), quiet=TRUE)
    # remove the first null patient
    ccd_ <<- ccRecord()
    ccd_ <<- ccd_ + ccd[2, 1] + ccd[3, 1]

    ccd2 <<- xml2Data2("../data/test_data_anonym.xml")


#}
