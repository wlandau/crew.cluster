test_that("crew_options_lsf()", {
  out <- crew_options_lsf(
    verbose = TRUE,
    command_submit = "submit",
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
