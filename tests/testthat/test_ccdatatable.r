context("Testing ccTable")
env <- environment()
ccd_delta <- suppressWarnings(deltaTime(ccd_, anonymised=T))
conf <- yaml.load_file('../data/test_spec.yml')
tb <- ccTable(record=ccd_delta, conf=conf)

test_that("test create table",{
    tb <- env$tb
    tb$create.table(freq=1)
    # assign table to both origin and clean table
    expect_true(!is.null(tb$torigin))
    expect_equivalent(tb$torigin, tb$tclean)
})

test_that("test get.missingness", {
    cr <-
        ccRecord()+ccEpisode(list(NIHR_HIC_ICU_0108=data.frame(time=as.numeric(seq(100)),
                                                       item2d=as.character(rep(10,100)))))
    tb <- ccTable(record=cr, conf=yaml.load_file('../data/test_2yml.yml'))
    tb$create.table(freq=1)
    tb$conf[[1]][['missingness_2d']][['labels']][['yellow']] <- 1
    tb$get.missingness()
    expect_equal(tb$dquality$missingness$NIHR_HIC_ICU_0108.yellow, 100/101*100)

    tb$conf[[1]][['missingness_2d']][['labels']][['yellow']] <- 0.1
    tb$get.missingness()
    expect_equal(tb$dquality$missingness$NIHR_HIC_ICU_0108.yellow, 100/1001*100)


})

test_that("test filter missingness", {
    tb <- env$tb
    tb$filter.missingness()
    expect_true(any(class(tb$tclean)=="data.table"))
    timestamps <- tb$tclean[, .N , by=c("site", "episode_id")]
    for(i in names(tb$conf)) {
        nastamps <- tb$tclean[, length(which(as.character(.SD[[i]]) != "NA")),
                               by=c("site", "episode_id")]
        nonmiss <- as.numeric(as.character(nastamps$V1)) /
            as.numeric(as.character(timestamps$N))*100
        accept <-
            as.numeric(tb$conf[[i]][["missingness_2d"]][["accept_2d"]])
        if (length(accept) != 0)
            expect_true(nonmiss[1] >= accept)
    }
})

test_that("test the case where no tclean or missingness been
          initialised.", 
{
    tb <- env$tb
    tclean <- tb$tclean
    # check the case when there is no missingness table
    tb$dquality$missingness <- data.table(NULL)
    tb$tclean <- data.table(NULL)
    tb$filter.missingness()
    expect_true(any(class(tb$tclean)=="data.table"))
    expect_equivalent(tclean, tb$tclean)
})

test_that("check the recount argument", {})


test_that("test imputation", 
{
    tb <- env$tb
    tb$imputation()
})


test_that("test range check", 
{
    tb <- env$tb
    tb$tclean <- tb$torigin
    tb$filter.ranges()
# case1 : no range specified in yml
# case2 : missing range speicification 
# case3 : overlapping, i.e. accept and impossible should not overlap.
})


test_that("test categorical data filter", 
{
    tb <- env$tb
    tb$check.categorical()
    tb$filter.categorical()

    tt <<- tb
})
