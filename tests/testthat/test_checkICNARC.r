ontext("Tests for ICNARC-related functions")

test_that("validate codes",
{
    expect_true(is.valid.icnarc.code('1'))
    expect_true(is.valid.icnarc.code('1.1'))
    expect_true(is.valid.icnarc.code('1.1.4'))
    expect_true(is.valid.icnarc.code('1.1.4.27'))
    expect_true(is.valid.icnarc.code('1.1.4.27.5'))
    
    expect_true(is.valid.icnarc.code('2'))
    expect_true(is.valid.icnarc.code('2.1'))
    expect_true(is.valid.icnarc.code('2.1.4'))
    expect_true(is.valid.icnarc.code('2.1.4.27'))
    expect_true(is.valid.icnarc.code('2.1.4.27.5'))
    
    expect_equal(is.valid.icnarc.code('3'), FALSE)
    expect_equal(is.valid.icnarc.code('1.99.123'), FALSE)
    expect_equal(is.valid.icnarc.code('1.1.4.27.SPIROS'), FALSE)
    expect_equal(is.valid.icnarc.code('1.1.4.27.5.1'), FALSE)
    expect_equal(is.valid.icnarc.code('2.01.04.27.05'), FALSE)
    
    expect_equal(normalize.icnarc.code('1.08.07.20.01'), '1.8.7.20.1')
    expect_equal(normalize.icnarc.code('1.8.7.20.1'), '1.8.7.20.1')
    expect_equal(normalize.icnarc.code('1.08'), '1.8')
    expect_equal(normalize.icnarc.code('1.8'), '1.8')
})
