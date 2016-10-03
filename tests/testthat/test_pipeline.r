context("data parsing pipeline")

test.setup <- function() {
    for (i in c(".temp", ".temp/XML")) {
        unlink(i, recursive=T)
        dir.create(i)
    }
}

test.teardown <- function() {
    unlink(".temp", recursive=T)
}


test_that("Discover parsed XML files and new files", {
    test.setup()
    f <- find.new.xml.file(".temp/XML")
    expect_true(dir.exists(".temp/XML/.database"))
    expect_length(f, 0)

    file.create(".temp/XML/test1.xml")
    file.create(".temp/XML/test2.xml")
    
    f <- find.new.xml.file(".temp/XML")
    expect_equivalent(f, c("test1.xml", "test2.xml"))

    file.create(".temp/XML/.database/test1.xml_someothersuffix.RData")
    f <- find.new.xml.file(".temp/XML")
    expect_equivalent(f, c("test2.xml"))

    f <- find.new.xml.file(".temp/XML", restart=TRUE)
    expect_equivalent(f, c("test1.xml", "test2.xml"))


    file.create(".temp/XML/test1.xml_part1")
    file.create(".temp/XML/test1.xml_part2")
    file.create(".temp/XML/test1.xml_part3")
    f <- find.new.xml.file(".temp/XML")
    expect_equivalent(f, "test2.xml")
    
    file.create(".temp/XML/test2.xml_part1")
    file.create(".temp/XML/test2.xml_part2")
    file.create(".temp/XML/test2.xml_part3")
    f <- find.new.xml.file(".temp/xml")

    # should pass with XML as suffix
    file.create(".temp/XML/file.XML")
    find.new.xml.file(".temp/xml")

    file.create(".temp/XML/not_end_with_xmlsuffix")
    expect_error(find.new.xml.file(".temp/xml"))
    file.remove(".temp/XML/not_end_with_xmlsuffix")

    test.teardown()
})


test_that("update the new XML files", {
    test.setup()
    system("cp ../data/test_data_anonym.xml .temp/XML/test1.xml")
    system("cp ../data/test_data_anonym.xml .temp/XML/test2.xml")
    new.db <- update.new.xml(".temp/XML")
    expect_is(new.db, "ccRecord")

    xml1 <- xml2Data(".temp/xml/test1.xml")
    xml2 <- xml2Data(".temp/xml/test2.xml")

    expect_equal(new.db@nepisodes, xml1@nepisodes + xml2@nepisodes)
    expect_true("test1.xml.RData" %in% dir(".temp/XML/.database"))
    expect_true("test2.xml.RData" %in% dir(".temp/XML/.database"))
})

test_that("update the database", {
    alld <- update.database(".temp/XML")
    
    expect_true("alldata.RData" %in% dir(".temp/XML/.database"))
})


test_that("Tear down the test", {
    test.teardown()
})
