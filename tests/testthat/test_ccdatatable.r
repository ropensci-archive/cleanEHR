context("Testing ccDataTable")
env <- environment()
ccd_delta <- suppressWarnings(deltaTime(ccd_, anonymised=T))
conf <- yaml.load_file('../data/test_yml.yml')
tb <- ccDataTable2(record=ccd_delta, conf=conf)

test_that("test create table",{
    tb <- env$tb
    tb$create.table(freq=1)
    # assign table to both origin and clean table
    expect_true(!is.null(tb$origin_table))
    expect_equivalent(tb$origin_table, tb$clean_table)
})

test_that("test count missingness", {
    tb <- env$tb
    tb$count.missingness()
})

test_that("test filter missingness", {
    tb <- env$tb
    tb$filter.missingness()
    expect_true(any(class(tb$clean_table)=="data.table"))
    timestamps <- tb$clean_table[, .N , by=c("site", "episode_id")]
    print(timestamps)
    for(i in names(tb$conf)) {
        nastamps <- tb$clean_table[, length(which(as.character(.SD[[i]]) == "NA")),
                               by=c("site", "episode_id")]
        print(nastamps)
    }

    





})

test_that("test the case where no clean_table or missingness_table been
          initialised.", 
{
    tb <- env$tb
    clean_table <- tb$clean_table
    # check the case when there is no missingness table
    tb$missingness_table <- data.table(NULL)
    tb$clean_table <- data.table(NULL)
    tb$filter.missingness()
    expect_true(any(class(tb$clean_table)=="data.table"))
    expect_equivalent(clean_table, tb$clean_table)
})

test_that("check the recount argument", {})
