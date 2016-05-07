context("Testing reallocate time variables")

test_that("unit test with data from 1-10",
{
   data <- data.frame(item2d=as.character(seq(10)), 
                      time=as.numeric(seq(10)))
   data_new <- reallocateTime(data, 11, 1)

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

