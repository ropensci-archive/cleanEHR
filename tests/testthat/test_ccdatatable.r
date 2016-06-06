library(data.table)
context("Testing ccDataTable")

test_that("test create table",{
    ccd_delta <- suppressWarnings(deltaTime(ccd_, anonymised=T))
    conf <- yaml.load_file('../data/test_yml.yml')
    tb <- ccDataTable2(record=ccd_delta, conf=conf)
    tb$create.table(freq=1)
#    print(tb$fun1())
    print(tb$filter.missingness())
    print(search())
    # assign table to both origin and clean table
    expect_true(!is.null(tb$origin_table))
    expect_equivalent(tb$origin_table, tb$clean_table)
})
