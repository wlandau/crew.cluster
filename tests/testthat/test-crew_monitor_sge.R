test_that("crew_monitor_sge() validate", {
  expect_silent(crew_monitor_sge()$validate())
})
