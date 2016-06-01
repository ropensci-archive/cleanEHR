context("Tests of the xml parser")

test_that("check original filename",
{
  # with no changes
  file_input_orig <- "/this/is/a/path/and_its_file.xml"
  expect_equal(extract_file_origin(file_input_orig),file_input_orig)

  # with a suffix
  file_input <- "/this/is/a/path/and_its_file.xml_00.part"
  expect_equal(extract_file_origin(file_input),file_input_orig)

  # with multiple xmls
  file_input <- "/this/is/a/path/and_its_file.xml_00.xml.part"
  expect_equal(extract_file_origin(file_input),file_input_orig)

  # extracting different extension
  file_input <- "/this/is/a/path/and_its_file.xml.txt.part"
  expect_equal(extract_file_origin(file_input, removestr='.txt'),
               paste(file_input_orig, ".txt", sep=""))
}


test_that("load xml file", 
{
# ccdata ccd is loaded as a global variable.
# note we faked the first patient as NULL
    expect_equal(ccd@npatient, 3)
    
    for (i in c(2, 3)) {
        for (item in names(ccd@patients[[i]]@episodes[[1]]@data)) {
            if (getItemInfo(item)['dt_code'] == "NULL")
                expect_true(
                    is.character(ccd@patients[[i]]@episodes[[1]]@data[item][[item]]))
            else
                expect_true(
                    is.data.frame(ccd@patients[[i]]@episodes[[1]]@data[item][[item]]))
        }
    }
})


test_that("check first patient heart rate (2d)",
{
    heart_rate_code <- getItemInfo("Heart rate")['NHIC_code']

    heart_rate <- 
        as.numeric(as.character(ccd@patients[[2]]@episodes[[1]]@data[heart_rate_code]$val))
    expect_true(all(heart_rate < 150))
})


test_that("check patients nhs number and pas number (1d)", 
{
    pas_code <- getItemInfo("PAS number")['NHIC_code']
    nhs_code <- getItemInfo("NHS number")['NHIC_code']
    
    pas_number_1 <- as.character(ccd@patients[[2]]@episodes[[1]]@data[pas_code])
    nhs_number_1 <- as.character(ccd@patients[[2]]@episodes[[1]]@data[nhs_code])
    pas_number_2 <- as.character(ccd@patients[[3]]@episodes[[1]]@data[pas_code])
    nhs_number_2 <- as.character(ccd@patients[[3]]@episodes[[1]]@data[nhs_code])

    expect_match(pas_number_1, "58014f10860311e4ae76005056b34847")
    expect_match(pas_number_2, "5813e3a0860311e4ae76005056b34847")
    expect_match(nhs_number_1, "57fb752c860311e4ae76005056b34847")
    expect_match(nhs_number_2, "NULL") 
})


test_that("test searching",
{

})
