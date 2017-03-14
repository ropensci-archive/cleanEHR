context("Testing ccRecord")

test_that("test ccEpisode", {
    eps <- new.episode(list())
    
    expect_equal(eps@nhs_number, "NA")
    expect_equal(eps@pas_number, "NA")
    expect_equal(eps@site_id, "NA")
    expect_equal(eps@episode_id, "NA")
    expect_equal(eps@t_admission, as.POSIXct(NA))
    expect_equal(eps@t_discharge, as.POSIXct(NA))
    
    xml <- xmlLoad('../data/test_data_anonym.xml')
    xmleps <- xmlEpisodeToList(getXmlepisode(xml, 1))
    eps <- new.episode(xmleps)
    expect_equal(eps@nhs_number, "nhs_1")
    expect_equal(eps@pas_number, "pas_1")
})


test_that("reorder the index of record", {

})


test_that("reorder the index of record", {
    rec <- ccRecord()
    #ccd[1]

})


test_that("test xml2Data", {
    ccd <- xml2Data("../data/test_data_anonym.xml")
})


test_that("testing unique spell", {
    ccd <- ccRecord() + new.episode(list(NIHR_HIC_ICU_0073="NHS1",
                                         NIHR_HIC_ICU_0411="2000-01-01", 
                                         NIHR_HIC_ICU_0412="2000-01-03"))
    ccd <- ccd + new.episode(list(NIHR_HIC_ICU_0073="NHS1",
                                  NIHR_HIC_ICU_0411="2000-01-03", 
                                  NIHR_HIC_ICU_0412="2000-01-30"))
    
    expect_equivalent(unique_spell(ccd)$spell, c(1, 1))
    
    ccd <- ccRecord() + new.episode(list(NIHR_HIC_ICU_0073="NHS1",
                                         NIHR_HIC_ICU_0411="2000-01-01", 
                                         NIHR_HIC_ICU_0412="2000-01-03"))
    ccd <- ccd + new.episode(list(NIHR_HIC_ICU_0073="NHS1",
                                  NIHR_HIC_ICU_0411="2000-01-04", 
                                  NIHR_HIC_ICU_0412="2000-01-30"))
   
    expect_equivalent(unique_spell(ccd)$spell, c(1, 1))


    # The second admission time earlier than the discharge time by mistake. 
    ccd <- ccRecord() + new.episode(list(NIHR_HIC_ICU_0073="NHS1",
                                         NIHR_HIC_ICU_0411="2000-01-01", 
                                         NIHR_HIC_ICU_0412="2000-01-03"))
    ccd <- ccd + new.episode(list(NIHR_HIC_ICU_0073="NHS1",
                                  NIHR_HIC_ICU_0411="2000-01-01", 
                                  NIHR_HIC_ICU_0412="2000-01-30"))

    expect_equivalent(unique_spell(ccd)$spell, c(1, 1))

})
