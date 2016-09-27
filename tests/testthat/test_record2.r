context("Testing new ccRecord")

test_that("test new.episode", {
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
    rec <- ccRecord2()
#    add.episode.to.record(rec, )

})


test_that("test xml2Data2", {
    ccd2 <<- xml2Data2("../data/test_data_anonym.xml")
})
