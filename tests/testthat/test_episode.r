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
