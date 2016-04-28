context("Testing sanity check")

test_that("configuration NHIC code not specified", 
{
# create one episode testing record.
    rt <- ccRecord() + ccEpisode(data=list(t1=10)) 
    conf <- list(t1=list(range=list(accept="1 - 10")))
    expect_error(rt1 <- check.sanity(rt, conf))
})

test_that("test sanity check on 1d data", 
{
    # test episode has 1d and 2d data
    rt <- ccRecord() + 
        ccEpisode(data=list(NIHR_HIC_ICU_0001="10")) 
    conf <- list(t1=list())
    conf$t1$NHIC <- "NIHR_HIC_ICU_0001"

    conf$t1$range$accept <- "1 - 5" # rejected
    rc <- check.sanity(rt, conf)
    expect_equal(rc@data_quality[[1]][[1]][["NIHR_HIC_ICU_0001"]], 0)

    conf$t1$range$accept <- "1 - 10" # accepted
    rc <- check.sanity(rt, conf)
    expect_equal(rc@data_quality[[1]][[1]][["NIHR_HIC_ICU_0001"]], 2)


    conf$t1$range$accept <- "1 - 5"
    conf$t1$range$indeterminable <- "0 - 10"
    rc <- check.sanity(rt, conf)
    expect_equal(rc@data_quality[[1]][[1]][["NIHR_HIC_ICU_0001"]], 1)

    conf$t1$range$indeterminable <- "1 - 20"
    conf$t1$range$accept <- "1 - 15"
    conf$t1$range$normal <- "9 - 10"
    rc <- check.sanity(rt, conf)
    expect_equal(rc@data_quality[[1]][[1]][["NIHR_HIC_ICU_0001"]], 3)

})


test_that("2d data", 
{
    rt <- ccRecord() + 
        ccEpisode(data=list(NIHR_HIC_ICU_0180=data.frame(time=seq(10), 
                                          item2d=seq(10))))
    conf <- list(t2=list())
    conf$t2$NHIC <- "NIHR_HIC_ICU_0180"
    conf$t2$range$accept <- "1 - 10"
    rc <- check.sanity(rt, conf)
    expect_equivalent(rc@data_quality[[1]][[1]][["NIHR_HIC_ICU_0180"]], rep(2, 10))

    conf$t2$range$accept <- "1 - 9"
    rc <- check.sanity(rt, conf)
    expect_equivalent(rc@data_quality[[1]][[1]][["NIHR_HIC_ICU_0180"]], c(rep(2, 9), 0))

    conf$t2$range$accept <- "1 - 9"
    conf$t2$range$normal <- "1 - 1"
    rc <- check.sanity(rt, conf)
    expect_equivalent(rc@data_quality[[1]][[1]][["NIHR_HIC_ICU_0180"]], c(3, rep(2, 8), 0))
})


test_that("range not specified", 
{
    rt <- ccRecord() + 
        ccEpisode(data=list(t2=data.frame(time=seq(10), 
                                          item2d=seq(10))))
    conf <- list(t2=list())
    conf$t2$NHIC <- "NIHR_HIC_ICU_0180"
    rc <- check.sanity(rt, conf)
    expect_true(is.list(rc@data_quality[[1]][[1]]))
    expect_true(is.null(rc@data_quality[[1]][[1]][["NIHR_HIC_ICU_0180"]]))
})


test_that("NHIC code Error", 
{
    rt <- ccRecord() + 
        ccEpisode(data=list(t2=data.frame(time=seq(10), 
                                          item2d=seq(10))))
    conf <- list(t2=list())
    conf$t2$NHIC <- "wrong_nhic"
    conf$t2$range$accept <- "1 - 10"
    expect_error(check.sanity(rt, conf))
})


test_that("check item doesn't exist in the data", 
{


})

test_that("range definition logical error", 
{

})

