test_that("valid simple crew_launcher_pbs()", {
  expect_silent(crew_launcher_pbs())
})

test_that("valid populated crew_launcher_pbs()", {
  expect_silent(
    crew_launcher_pbs(
      options_cluster = crew_options_pbs(
        script_lines = c("module load R", "echo 'start'"),
        log_output = "out",
        log_error = "err",
        log_join = FALSE,
        memory_gigabytes_required = 2,
        cores = 2L,
        walltime_hours = 1L
      )
    )
  )
})

test_that("invalid crew_launcher_pbs(): pbs field", {
  x <- crew_launcher_pbs()
  private <- crew_private(x)
  private$.options_cluster$cores <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_pbs() script() nearly empty", {
  x <- crew_launcher_pbs(
    options_cluster = crew_options_pbs(
      cwd = FALSE,
      log_output = "log_file",
      log_join = FALSE,
      walltime_hours = NULL
    )
  )
  expect_equal(
    x$script(name = "my_job"),
    c("#PBS -N my_job", "#PBS -o log_file", "#PBS -j n")
  )
})

test_that("crew_launcher_pbs() script() all lines", {
  x <- crew_launcher_pbs(
    options_cluster = crew_options_pbs(
      script_lines = c("module load R", "echo 'start'"),
      log_output = "out_dir/",
      log_error = "err_dir/",
      log_join = FALSE,
      memory_gigabytes_required = 2,
      cores = 2L,
      walltime_hours = 57
    )
  )
  out <- x$script(name = "this_job")
  exp <- c(
    "#PBS -N this_job",
    "#PBS -o out_dir/",
    "#PBS -e err_dir/",
    "#PBS -j n",
    "#PBS -l mem=2gb",
    "#PBS -l ppn=2",
    "#PBS -l walltime=57:00:00",
    "module load R",
    "echo 'start'",
    "cd \"$PBS_O_WORKDIR\""
  )
  expect_equal(out, exp)
})

test_that("deprecate command_delete", {
  skip_on_cran()
  suppressWarnings(
    expect_warning(
      x <- crew_launcher_pbs(command_delete = "user_del"),
      class = "crew_deprecate"
    )
  )
  expect_equal(x$options_cluster$command_terminate, "user_del")
})
