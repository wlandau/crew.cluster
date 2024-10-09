test_that("crew_options_cluster", {
  out <- crew_options_cluster(script_directory = "x")
  expect_equal(
    out,
    structure(
      list(
        verbose = FALSE,
        command_submit = "",
        command_terminate = "",
        script_directory = "x",
        script_lines = character(0)
      ),
      class = c("crew_options_cluster", "crew_options")
    )
  )
})
