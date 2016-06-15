context("Testing readOneItem")

test_that("read time series data (data2d)", 
{
    table <- readOneItem(ccd, "NIHR_HIC_ICU_0108")

    p1_heartr <- ccd@patients[[2]]@episodes[[1]]@data$NIHR_HIC_ICU_0108
    p2_heartr <- ccd@patients[[3]]@episodes[[1]]@data$NIHR_HIC_ICU_0108

    l1 <- nrow(p1_heartr)
    l2 <- nrow(p2_heartr)

    expect_identical(table$data2d$episode_id, c(rep("20140200", l1),
                                                rep("20140201", l2)))
    expect_identical(table$data2d$nhs_number,
                     c(rep(ccd@patients[[2]]@episodes[[1]]@nhs_number, l1),
                       rep(ccd@patients[[3]]@episodes[[1]]@nhs_number, l2)))

    expect_identical(table$data2d$pas_number,
                     c(rep(ccd@patients[[2]]@episodes[[1]]@pas_number, l1),
                       rep(ccd@patients[[3]]@episodes[[1]]@pas_number, l2)))

    expect_identical(as.character(table$data2d$time), c(as.character(p1_heartr$time),
                                                        as.character(p2_heartr$time)))

    expect_identical(as.character(table$data2d$val),
                     c(as.character(p1_heartr$item2d),
                       as.character(p2_heartr$item2d)))
})

