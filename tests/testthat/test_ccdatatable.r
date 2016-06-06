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
    tt <<- tb
})
