test_that("crew_options_slurm()", {
  out <- crew_options_slurm(
    verbose = TRUE,
    command_submit = "submit",
    command_terminate = "terminate",
    script_directory = "a",
    script_lines = "b",
    log_output = "/dev/null",
    log_error = "/dev/null",
    memory_gigabytes_required = 123,
    memory_gigabytes_per_cpu = 456,
    cpus_per_task = 3,
    time_minutes = 1440,
    partition = "partition"
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
        log_output = "/dev/null",
        log_error = "/dev/null",
        memory_gigabytes_per_cpu = 456,
        memory_gigabytes_required = 123,
        cpus_per_task = 3,
        time_minutes = 1440,
        partition = "partition"
      ),
      class = c("crew_options_slurm", "crew_options_cluster", "crew_options")
    )
  )
})

test_that("slurm retryable options", {
  options <- crew_options_slurm(
    verbose = TRUE,
    command_submit = "sbatch",
    command_terminate = NULL,
    script_directory = "script_directory",
    script_lines = c("module", "load", "R", "version", "4.4.0"),
    log_output = "output_log",
    log_error = "error_log",
    memory_gigabytes_required = c(1, 1.1, 2.3),
    memory_gigabytes_per_cpu = c(4, 5, 6),
    cpus_per_task = c(1, 2),
    time_minutes = c(300, 201, 150),
    partition = c("partition1", "partition2")
  )
  expect_equal(
    crew_options_slice(options, 1L),
    crew_options_slurm(
      verbose = TRUE,
      command_submit = "sbatch",
      command_terminate = NULL,
      script_directory = "script_directory",
      script_lines = c("module", "load", "R", "version", "4.4.0"),
      log_output = "output_log",
      log_error = "error_log",
      memory_gigabytes_required = 1,
      memory_gigabytes_per_cpu = 4,
      cpus_per_task = 1,
      time_minutes = 300,
      partition = "partition1"
    )
  )
  expect_equal(
    crew_options_slice(options, 2L),
    crew_options_slurm(
      verbose = TRUE,
      command_submit = "sbatch",
      command_terminate = NULL,
      script_directory = "script_directory",
      script_lines = c("module", "load", "R", "version", "4.4.0"),
      log_output = "output_log",
      log_error = "error_log",
      memory_gigabytes_required = 1.1,
      memory_gigabytes_per_cpu = 5,
      cpus_per_task = 2,
      time_minutes = 201,
      partition = "partition2"
    )
  )
  for (index in seq(3L, 6L)) {
    expect_equal(
      crew_options_slice(options, index),
      crew_options_slurm(
        verbose = TRUE,
        command_submit = "sbatch",
        command_terminate = NULL,
        script_directory = "script_directory",
        script_lines = c("module", "load", "R", "version", "4.4.0"),
        log_output = "output_log",
        log_error = "error_log",
        memory_gigabytes_required = 2.3,
        memory_gigabytes_per_cpu = 6,
        cpus_per_task = 2,
        time_minutes = 150,
        partition = "partition2"
      )
    )
  }
})
