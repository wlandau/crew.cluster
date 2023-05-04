test_that("name_job()", {
  expect_equal(name_job("a", "b", "c"), "a-b-c")
  expect_equal(name_job("1", "b", "c"), "worker-1-b-c")
})

test_that("path_script()", {
  script <- path_script(dir = "x", prefix = "a", launcher = "b", worker = "c")
  expect_equal(script, file.path("x", "a-b-c.sh"))
})
