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

test_that("crew_options_slice() respects non-retryable options", {
  options <- crew_options_slurm(
    verbose = TRUE,
    command_submit = "sbatch",
    command_terminate = NULL,
    script_directory = "script_directory",
    script_lines = c("module", "load", "R", "version", "4.4.0"),
    log_output = "output_log",
    log_error = "error_log",
    memory_gigabytes_required = NULL,
    memory_gigabytes_per_cpu = NULL,
    cpus_per_task = NULL,
    time_minutes = NULL,
    partition = NULL
  )
  for (index in seq_len(3)) {
    slice <- crew_options_slice(options, index)
    expect_true(all(names(slice) %in% names(options)))
    expect_equal(class(slice), class(options))
    for (name in names(options)) {
      expect_equal(slice[[name]], options[[name]])
    }
  }
})
