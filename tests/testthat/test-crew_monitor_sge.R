test_that("crew_monitor validate", {
  expect_silent(crew_monitor_sge()$validate())
})

test_that("crew_monitor active bindings", {
  x <- crew_monitor_sge(
    verbose = FALSE,
    command_list = "x",
    command_terminate = "y"
  )
  expect_false(x$verbose)
  expect_equal(x$command_list, "x")
  expect_equal(x$command_terminate, "y")
})
