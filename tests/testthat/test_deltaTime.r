context("Testing deltaTime")

#test_that("convert to delta time and convert it back",
#{
#    dt <- deltaTime(ccd, anonymised=TRUE, tdiff=TRUE)
#    time_restore <- for_each_episode(dt, 
#                     function(episode) {
#                         env <- environment() 
#                         lapply(episode@data,
#                                function(x) {
#                                    if (length(x)>1) {
#                                        env$episode@admin_icu_time + x$time
#                                    }
#                                })
#                     })
#    time_origin <- for_each_episode(ccd, 
#                     function(episode) {
#                         lapply(episode@data,
#                                function(x) {
#                                    if (length(x)>1) {
#                                        xmlTime2POSIX(x$time)
#                                    }
#                                })
#                     })
#    expect_equal(dt@npatient, 2)
#    # first episode (patient) is NULL, it has been removed, that why here we
#    # have 1 index shift.
#    expect_equivalent(time_origin[2:3], time_restore[1:2])
#})
#
#
#test_that("episodes where admission time is missing", {
#    rc <- ccRecord() + ccEpisode() + ccEpisode()
#    rc@patients[[1]]@episodes[[1]]@admin_icu_time<-"2014-02-01T16:00:00"
#    rc@patients[[2]]@episodes[[1]]@admin_icu_time<-"NULL"
#    expect_equal(rc@npatient, 2)
#    expect_equal(deltaTime(rc)@npatient, 1)
#})


test_that("convert to delta time and recover it back", {
    ddd <- deltaTime(ccd2, anonymised=TRUE, tdiff=TRUE)
})
