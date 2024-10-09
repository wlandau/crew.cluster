test_that("crew_options_pbs()", {
  out <- crew_options_pbs(
    verbose = TRUE,
    command_submit = "submit",
    command_terminate = "terminate",
    script_directory = "a",
    script_lines = "b",
    cwd = TRUE,
    log_output = "/dev/null2",
    log_error = "abc",
    log_join = FALSE,
    memory_gigabytes_required = 123,
    cores = 3,
    walltime_hours = 24
  )
  expect_equal(
    out,
    structure(
      list(
        verbose = TRUE,
        command_submit = "submit",
        command_terminate = "terminate", 
        script_directory = "a",
        script_lines = "b",
        cwd = TRUE,
        log_output = "/dev/null2", 
        log_error = "abc",
        log_join = FALSE,
        memory_gigabytes_required = 123,
        cores = 3,
        walltime_hours = 24
      ),
      class = c("crew_options_pbs", "crew_options_cluster", "crew_options")
    )
  )
})
