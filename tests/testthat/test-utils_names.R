test_that("name_job()", {
  expect_equal(name_job("a", "b", "c"), "a-b-c")
  expect_equal(name_job("1", "b", "c"), "sge-1-b-c")
})
