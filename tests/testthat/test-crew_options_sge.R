test_that("crew_options_sge()", {
  out <- crew_options_sge(
    verbose = TRUE,
    command_submit = "submit",
    command_terminate = "terminate",
    script_directory = "a",
    script_lines = "b",
    cwd = FALSE,
    envvars = TRUE,
    log_output = "/dev/null1",
    log_error = "/dev/null2",
    log_join = FALSE,
    memory_gigabytes_limit = 456,
    memory_gigabytes_required = 123,
    cores = 4,
    gpu = 2
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
        cwd = FALSE,
        envvars = TRUE,
        log_output = "/dev/null1",
        log_error = "/dev/null2",
        log_join = FALSE,
        memory_gigabytes_limit = 456,
        memory_gigabytes_required = 123,
        cores = 4,
        gpu = 2
      ),
      class = c("crew_options_sge", "crew_options_cluster", "crew_options")
    )
  )
})

test_that("sge retryable options", {
  options <- crew_options_sge(
    verbose = TRUE,
    command_submit = "submit",
    command_terminate = "terminate",
    script_directory = "a",
    script_lines = "b",
    cwd = FALSE,
    envvars = TRUE,
    log_output = "log1",
    log_error = "log2",
    log_join = FALSE,
    memory_gigabytes_limit = c(234, 456),
    memory_gigabytes_required = c(11, 33, 22),
    cores = c(4, 2),
    gpu = c(2, 1, 2)
  )
  expect_equal(
    crew_options_slice(options, 1L),
    crew_options_sge(
      verbose = TRUE,
      command_submit = "submit",
      command_terminate = "terminate",
      script_directory = "a",
      script_lines = "b",
      cwd = FALSE,
      envvars = TRUE,
      log_output = "log1",
      log_error = "log2",
      log_join = FALSE,
      memory_gigabytes_limit = 234,
      memory_gigabytes_required = 11,
      cores = 4,
      gpu = 2
    )
  )
  expect_equal(
    crew_options_slice(options, 2L),
    crew_options_sge(
      verbose = TRUE,
      command_submit = "submit",
      command_terminate = "terminate",
      script_directory = "a",
      script_lines = "b",
      cwd = FALSE,
      envvars = TRUE,
      log_output = "log1",
      log_error = "log2",
      log_join = FALSE,
      memory_gigabytes_limit = 456,
      memory_gigabytes_required = 33,
      cores = 2,
      gpu = 1
    )
  )
  for (index in seq(3L, 6L)) {
    expect_equal(
      crew_options_slice(options, index),
      crew_options_sge(
        verbose = TRUE,
        command_submit = "submit",
        command_terminate = "terminate",
        script_directory = "a",
        script_lines = "b",
        cwd = FALSE,
        envvars = TRUE,
        log_output = "log1",
        log_error = "log2",
        log_join = FALSE,
        memory_gigabytes_limit = 456,
        memory_gigabytes_required = 22,
        cores = 2,
        gpu = 2
      )
    )
  }
})
