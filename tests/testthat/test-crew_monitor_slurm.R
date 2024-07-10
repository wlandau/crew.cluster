test_that("crew_monitor_slurm() validate", {
  expect_silent(crew_monitor_slurm()$validate())
})
