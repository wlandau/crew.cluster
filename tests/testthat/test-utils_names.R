test_that("name_job()", {
  expect_equal(name_job("a", "b", "c"), "a-b-c")
  expect_equal(name_job("1", "b", "c"), "worker-1-b-c")
})

test_that("name_script()", {
  name <- name_job("a", "b", "c")
  script <- name_script(name)
  expect_equal(script, file.path(tempdir(), "a-b-c.sh"))
})
