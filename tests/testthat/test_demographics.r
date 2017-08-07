context("Testing demographic table")

test_that("create demographic table from ccdata and expect the equivalent
          results with or without data type awareness.", {
    demg <- demographic.table(ccd, dtype=F)
    expect_equal(nrow(demg), ccd@nepisodes)
    expect_match(class(demg)[1], "data.table")

    convert.back.to.char <- function(v) {
        v <- as.character(v)
        v[v=="NA"] <- "NULL"
        v[is.na(v)] <- "NULL"
        v
    }

    demg_t <- suppressWarnings(demographic.table(ccd))
    for (i in seq(ncol(demg) - 1))
        expect_equivalent(demg[[i]], convert.back.to.char(demg_t[[i]]))
}) 



context("Testing functionalities for the demographic table")

test_that("calculate length of stay in the ICU",{
    demg <- data.table(DAICU="2007-01-01", DDICU="2007-01-02")
    demg_ <- lenstay(demg)
    expect_true(is.data.frame(demg_))
    expect_equal(ncol(demg) + 1, ncol(demg_))
    expect_equal(as.numeric(demg_$lenstay), 24)

    demg <- data.table(DAICU="2007-01-01T00:00:00", DDICU="2007-01-01T22:00:00")
    demg_ <- lenstay(demg)
    expect_equal(as.numeric(demg_$lenstay), 22)

    demg <- data.table(DAICU="2007-01-01", DDICU="wrong_format")
    demg_ <- lenstay(demg)
    expect_true(is.na(demg_$lenstay))

    demg <- data.table(DAICU="wrong_format1", DDICU="wrong_format1")
    demg_ <- lenstay(demg)
    expect_true(is.na(demg_$lenstay))

    demg <- data.table(DAICU="2007-01-01T00:00:00", DDICU="2007-01-01T22:00:00")
    demg_ <- lenstay(demg, "days")
    expect_equal(as.numeric(demg_$lenstay), 1-2/24)
})

