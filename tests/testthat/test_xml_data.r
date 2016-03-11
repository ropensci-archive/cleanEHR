context("Tests of the xml parser")


test_that("load xml file", 
{

    r <- xmlLoad("../../../CriticalCare/anon_CC.xml")
    ccd <-xml2Data(r, seq(2), quiet=TRUE)
})
