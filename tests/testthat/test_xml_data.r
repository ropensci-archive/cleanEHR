context("Tests of the xml parser")

test_that("load xml file", 
{
# ccdata ccd is loaded as a global variable.
    expect_equal(ccd@npatient, 2)
    
    for (i in seq(2)) {
        for (item in names(ccd@patients[[i]]@episodes[[1]]@data)) {
            if (getItemInfo(item)['dt_code'] == "NULL")
              if (ccdata.env$ITEM_REF[[item]]$Datatype != 'numeric')
                expect_true(
                  is.character(ccd@patients[[i]]@episodes[[1]]@data[item][[item]]))
              else
                expect_true(
                  is.numeric(ccd@patients[[i]]@episodes[[1]]@data[item][[item]]))
            else
                expect_true(
                    is.data.frame(ccd@patients[[i]]@episodes[[1]]@data[item][[item]]))
        }
    }
})


test_that("check first patient heart rate (2d)",
{
    heart_rate_code <- getItemInfo("Heart rate")['NHIC_code']

    heart_rate <- ccd@patients[[1]]@episodes[[1]]@data[heart_rate_code]$val
    expect_true(all(heart_rate < 150))
})


test_that("check patients nhs number and pas number (1d)", 
{
    pas_code <- getItemInfo("PAS number")['NHIC_code']
    nhs_code <- getItemInfo("NHS number")['NHIC_code']
    
    pas_number_1 <- ccd@patients[[1]]@episodes[[1]]@data[pas_code][[1]]
    nhs_number_1 <- ccd@patients[[1]]@episodes[[1]]@data[nhs_code][[1]]
    pas_number_2 <- ccd@patients[[2]]@episodes[[1]]@data[pas_code][[1]]
    nhs_number_2 <- ccd@patients[[2]]@episodes[[1]]@data[nhs_code][[1]]

    expect_match(pas_number_1, "58014f10860311e4ae76005056b34847")
    expect_match(pas_number_2, "5813e3a0860311e4ae76005056b34847")
    expect_match(nhs_number_1, "57fb752c860311e4ae76005056b34847")
    expect_null(nhs_number_2)
})


test_that("test searching",
{

})
