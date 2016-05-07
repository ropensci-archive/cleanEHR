context("Testing deltaTime")

test_that("convert to delta time and convert it back",
{
    dt <- deltaTime(ccd, anonymised=TRUE, tdiff=TRUE)
    time_restore <- for_each_episode(dt, 
                     function(episode) {
                         env <- environment() 
                         lapply(episode@data,
                                function(x) {
                                    if (length(x)>1) {
                                        env$episode@admin_icu_time + x$time
                                    }
                                })
                     })

    time_origin <- for_each_episode(ccd, 
                     function(episode) {
                         lapply(episode@data,
                                function(x) {
                                    if (length(x)>1) {
                                        xmlTime2POSIX(x$time)
                                    }
                                })
                     })
    expect_equivalent(time_origin, time_restore)
})
