test_that("crew_options_lsf()", {
  out <- crew_options_lsf(
    verbose = TRUE,
    command_submit = "submit",
    command_terminate = "terminate",
    script_directory = "a",
    script_lines = "b",
    cwd = "x",
    log_output = "1",
    log_error = "2",
    memory_gigabytes_limit = 1234,
    memory_gigabytes_required = 123,
    cores = 2
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
        cwd = "x",
        log_output = "1",
        log_error = "2",
        memory_gigabytes_limit = 1234,
        memory_gigabytes_required = 123,
        cores = 2
      ),
      class = c("crew_options_lsf", "crew_options_cluster", "crew_options")
    )
  )
})

test_that("lsf retryable options", {
  options <- crew_options_lsf(
    verbose = TRUE,
    command_submit = "submit",
    command_terminate = "terminate",
    script_directory = "a",
    script_lines = "b",
    cwd = "x",
    log_output = "1",
    log_error = "2",
    memory_gigabytes_limit = c(1, 11, 1234),
    memory_gigabytes_required = c(3, 123),
    cores = c(3, 2)
  )
  expect_equal(
    crew_options_slice(options, 1L),
    crew_options_lsf(
      verbose = TRUE,
      command_submit = "submit",
      command_terminate = "terminate",
      script_directory = "a",
      script_lines = "b",
      cwd = "x",
      log_output = "1",
      log_error = "2",
      memory_gigabytes_limit = 1,
      memory_gigabytes_required = 3,
      cores = 3
    )
  )
  expect_equal(
    crew_options_slice(options, 2L),
    crew_options_lsf(
      verbose = TRUE,
      command_submit = "submit",
      command_terminate = "terminate",
      script_directory = "a",
      script_lines = "b",
      cwd = "x",
      log_output = "1",
      log_error = "2",
      memory_gigabytes_limit = 11,
      memory_gigabytes_required = 123,
      cores = 2
    )
  )
  for (index in seq(3L, 6L)) {
    expect_equal(
      crew_options_slice(options, index),
      crew_options_lsf(
        verbose = TRUE,
        command_submit = "submit",
        command_terminate = "terminate",
        script_directory = "a",
        script_lines = "b",
        cwd = "x",
        log_output = "1",
        log_error = "2",
        memory_gigabytes_limit = 1234,
        memory_gigabytes_required = 123,
        cores = 2
      )
    )
  }
})
