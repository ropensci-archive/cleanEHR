context("Testing ccDataTable")
env <- environment()
ccd_delta <- suppressWarnings(deltaTime(ccd_, anonymised=T))
conf <- yaml.load_file('../data/test_yml.yml')
tb <- ccDataTable2(record=ccd_delta, conf=conf)

test_that("test create table",{
    tb <- env$tb
    tb$create.table(freq=1)
    # assign table to both origin and clean table
    expect_true(!is.null(tb$torigin))
    expect_equivalent(tb$torigin, tb$tclean)
})

test_that("test get.missingness", {
    tb <- env$tb
    tb$get.missingness()
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
            as.numeric(tb$conf[[i]][["missingness_2d"]][["miss_acceptance"]])

        expect_true(nonmiss > accept)
    }
})

test_that("test the case where no tclean or missingness been
          initialised.", 
{
    tb <- env$tb
    tclean <- tb$tclean
    # check the case when there is no missingness table
    tb$missingness <- data.table(NULL)
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
    tt <<- tb

})
