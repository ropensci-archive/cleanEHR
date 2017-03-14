context("Testing utility functions")

test_that("break down ICNARC code", {
    expect_equivalent(icnarc.breakdown("1.2.3.4.5"), "1.2.3")
    expect_equivalent(icnarc.breakdown("1.2.3.4.5", 4), "1.2.3.4")

    expect_equivalent(icnarc.breakdown("10.2.3.4.5"), "10.2.3")
    expect_equivalent(icnarc.breakdown("01.2.3.4.5"), "1.2.3")
    
    expect_equivalent(icnarc.breakdown("1.2.3"), "1.2.3")
    expect_equivalent(icnarc.breakdown("1.2"), "1.2")
    expect_equivalent(icnarc.breakdown("1"), "1")
    
    expect_equivalent(icnarc.breakdown(c("1", "1.2.3.4")), c("1", "1.2.3"))
})


test_that("ICNARC Conversion",{
    expect_match(icnarc2diagnosis('1'), "Surgical")
    expect_match(icnarc2diagnosis('2'), "Nonsurgical")
    expect_true(icnarc2diagnosis("1.1") == "Respiratory (Surgical)")
    expect_true(icnarc2diagnosis("2.1") == "Respiratory (Nonsurgical)")
    expect_true(icnarc2diagnosis("02.01") == "Respiratory (Nonsurgical)")
    expect_true(icnarc2diagnosis("02.01", FALSE) == "Respiratory")
    expect_true(icnarc2diagnosis("02.01", levels=1) == "Nonsurgical")
})


