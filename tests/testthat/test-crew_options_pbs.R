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

test_that("pbs retryable options", {
  options <- crew_options_pbs(
    verbose = TRUE,
    command_submit = "submit",
    command_terminate = "terminate",
    script_directory = "a",
    script_lines = "b",
    cwd = TRUE,
    log_output = "/dev/null2",
    log_error = "abc",
    log_join = FALSE,
    memory_gigabytes_required = c(48, 123),
    cores = c(3, 2, 8),
    walltime_hours = c(24, 72)
  )
  expect_equal(
    crew_options_slice(options, 1L),
    crew_options_pbs(
      verbose = TRUE,
      command_submit = "submit",
      command_terminate = "terminate",
      script_directory = "a",
      script_lines = "b",
      cwd = TRUE,
      log_output = "/dev/null2",
      log_error = "abc",
      log_join = FALSE,
      memory_gigabytes_required = 48,
      cores = 3,
      walltime_hours = 24
    )
  )
  expect_equal(
    crew_options_slice(options, 2L),
    crew_options_pbs(
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
      cores = 2,
      walltime_hours = 72
    )
  )
  for (index in seq(3L, 6L)) {
    expect_equal(
      crew_options_slice(options, index),
      crew_options_pbs(
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
        cores = 8,
        walltime_hours = 72
      )
    )
  }
})
