test_that("invalid options", {
  expect_error(
    crew_controller_slurm(options_cluster = list()),
    class = "crew_error"
  )
  expect_error(
    crew_options_validate(list()),
    class = "crew_error"
  )
})
