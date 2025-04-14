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
    partition = "partition",
    n_tasks = 4
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
        partition = "partition",
        n_tasks = 4
      ),
      class = c("crew_options_slurm", "crew_options_cluster", "crew_options")
    )
  )
})
