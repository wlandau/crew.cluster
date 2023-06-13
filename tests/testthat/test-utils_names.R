test_that("path_script()", {
  script <- path_script(dir = "x", prefix = "a", launcher = "b", worker = "c")
  expect_equal(script, file.path("x", "a-b-c.sh"))
})
