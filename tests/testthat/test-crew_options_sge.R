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
        memory_gigabytes_required = 123, cores = 4, gpu = 2),
      class = c("crew_options_sge", "crew_options_cluster", "crew_options")
    )
  )
})
