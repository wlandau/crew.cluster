test_that("crew_monitor_cluster() validate", {
  expect_silent(crew_monitor_cluster()$validate())
})

test_that("crew_monitor_cluster() active bindings", {
  x <- crew_monitor_cluster(
    verbose = FALSE,
    command_list = "x",
    command_terminate = "y"
  )
  expect_false(x$verbose)
  expect_equal(x$command_list, "x")
  expect_equal(x$command_terminate, "y")
})
