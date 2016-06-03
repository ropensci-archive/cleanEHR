context("Testing episodes")

test_that("add 1D item.", {
    ep <- ccEpisode()
    expect_true(is.list(ep@data))
    expect_equal(length(ep@data), 0)
    ep2 <- ep + data.frame(id="xx", val="1")
    expect_match(ep2@data[["xx"]], "1")
    ep2 <- ep + data.frame(id=c("x", "x","y"),
                           time=c("tx1","tx2","ty"),
                           val=c("vx1","vx2", "vy"))
    expect_match(as.character(ep2@data[["x"]][2, 2]), "vx2")

    expect_error(ep + data.frame()) 
    expect_error(ep + data.frame("xx", 1)) # missing data frame labels 
    expect_error(ep + data.frame(id=c("xx", "xx"),
                                 val=c(1,2))) # duplication of 1d data
})

test_that("add an empty episode.", {
    new <- ccd + ccEpisode()
    expect_equal(new@npatient, ccd@npatient + 1)
})

test_that("check cases of duplicated ids", {
    # test PAS number
    p <- ccPatient()
    p <- p + ccEpisode(pas_number="pas_num_1")
    # newly added episode pas_number is 'NULL', and the pas_number should
    # maintain the original value.
    expect_match('pas_num_1', (p + ccEpisode())@pas_number)

    # test NHS number
    p2 <- ccPatient()
    p2 <- p2 + ccEpisode(nhs_number="nhs_num_1")
    expect_match('nhs_num_1', (p2 + ccEpisode())@nhs_number)
    expect_error(p2 + ccEpisode(nhs_number='nhs_number_2'))
})
