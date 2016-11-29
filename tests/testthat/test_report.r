context("Testing data quality report")


test_that("",{
    expect_error(table1(demg, "wrong_short_name"))
    table1(demg, c("ETHNIC", "LOCA"))
})

