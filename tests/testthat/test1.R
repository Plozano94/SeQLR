
context("Testing")

test_that("Query extraction works", {
    expect_equal(extract_query("../test.sqlr","chunk1")$query, "select * from users where id=${id}-- ")
    expect_equal(extract_query("../test.sqlr","chunk1")$name, "chunk1")
    expect_equal(extract_query("../test.sqlr","chunk1")$param$id, "2")
})

test_that("Query formatting works", {
    expect_equal(format_query(list("query" = "select * from users where id=${id}-- ",
                                   "name"="chunk1",
                                   "param"=list("id"="2")
                                   )
                             )
                , "select * from users where id=2-- ")
})
