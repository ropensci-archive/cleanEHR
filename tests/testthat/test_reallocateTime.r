context("Testing reallocate time variables")

test_that("unit test with data from 1-10",
{
   data <- data.frame(item2d=as.character(seq(10)), 
                      time=as.numeric(seq(10)))
   data_new <- reallocateTime(data, 10, 1)

   expect_equivalent(c(0:10), as.numeric(data_new$time))
   expect_equivalent(as.character(c("NA", seq(10))), as.character(data_new$val))

   data_new <- reallocateTime(data, 100, 0.1) 
   expect_equal(length(unique(data_new$val)), 11)
})

test_that("input and output data types", 
{
    data <- data.frame(seq(10), seq(10))
    expect_error(reallocateTime(data, 10, 1))

    data <- data.frame(item2d=seq(10), time=seq(10))
    expect_error(reallocateTime(data, 10, 1))
    
    data$item2d <- as.character(data$item2d)
    data$time <- as.character(data$time)
    expect_error(reallocateTime(data, 10, 1))

    data$item2d <- as.numeric(data$item2d)
    data$time <- as.numeric(data$time)
    out <- reallocateTime(data, 10, 1)

    data$item2d <- as.character(data$item2d)
    out <- reallocateTime(data, 10, .1)
}) 

test_that("check when item admin_icu_time or discharge_icu_time is missing", 
{
    # anonymised delta time used the same mechenism that searching first and
    # last data time stamp.
    dt <- deltaTime(ccd_, anonymised=TRUE)
    ta <- dt[1, 1]@admin_icu_time
    td <- dt[1, 1]@discharge_icu_time
   
    rr <- reallocateTimeRecord(dt)
    time <- rr[1, 1]@data[["NIHR_HIC_ICU_0108"]]$time
   
    #modify admin time to "NULL"
    dt@patients[[1]]@episodes[[1]]@admin_icu_time <- "NULL"
    rt <- reallocateTimeRecord(dt)
    time_with_null <- rt[1, 1]@data[["NIHR_HIC_ICU_0108"]]$time
    expect_equal(max(time_with_null), max(time))

    #modify discharge time to "NULL"
    dt@patients[[1]]@episodes[[1]]@discharge_icu_time <- "NULL"
    rt <- reallocateTimeRecord(dt)
    time_with_null <- rt[1, 1]@data[["NIHR_HIC_ICU_0108"]]$time
    expect_equal(max(time_with_null), max(time))

})

test_that("check data with meta data column (C++ function)", {
    input <- data.frame(time=seq(1, 100, 10), 
               item2d=rep("v", 10), 
               meta=rep("m", 10))
    output <- reallocateTime(input, 100, 1)
    # index increase by 1 
    expect_equal(nrow(output), 101)
    expect_equivalent(as.character(output[seq(2, 100, 10), 2]), rep("v", 10))
    expect_equivalent(as.character(output[seq(2, 100, 10), 3]), rep("m", 10))

    output <- reallocateTime(input, 100, 10)
    expect_equal(nrow(output), 11)

    output <- reallocateTime(input, 100, 0.1)
    expect_equal(nrow(output), 1001)
})
