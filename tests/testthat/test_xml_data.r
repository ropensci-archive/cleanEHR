context("Tests of the xml parser")

test_that("load xml file", 
{
    r <- xmlLoad("../data/test_data_10_patients.xml")
    ccd <- xml2Data(r, seq(1), quiet=TRUE)
    expect_equal(ccd@npatient, 1)
})
