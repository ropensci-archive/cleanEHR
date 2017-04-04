context("Testing SQL functionalities")

test_that("Export SQL variable tables from ccRecord", {
   expect_error(rec2tb('nonsense'))
   expect_true("data.frame" %in% class(rec2tb(ccd, "h_rate")))

   ltb <- export.lontb(ccd)
   expect_equal(class(ltb), "list")
   expect_true(all(stname2code(names(ltb)) != "NA"))
})


test_that("Create SQLite database", {
    suppressWarnings(sql.create.database(ccd))
    

})
