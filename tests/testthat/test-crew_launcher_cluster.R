test_that("valid abstract cluster launcher object", {
  expect_silent(crew_launcher_cluster())
})

test_that("bad field in cluster launcher object", {
  x <- crew_launcher_cluster()
  x$verbose <- 2L
  expect_error(x$validate(), class = "crew_error")
})
