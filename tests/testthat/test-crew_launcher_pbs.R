test_that("valid simple crew_launcher_pbs()", {
  skip_if_low_dep_versions()
  expect_silent(crew_launcher_pbs())
})

test_that("valid populated crew_launcher_pbs()", {
  skip_if_low_dep_versions()
  expect_silent(
    crew_launcher_pbs(
      script_lines = c("module load R", "echo 'start'"),
      pbs_log_output = "out",
      pbs_log_error = "err",
      pbs_log_join = FALSE,
      pbs_memory_gigabytes_required = 2,
      pbs_cores = 2L,
      pbs_walltime_hours = 1L
    )
  )
})

test_that("invalid crew_launcher_pbs(): pbs field", {
  skip_if_low_dep_versions()
  x <- crew_launcher_pbs()
  x$pbs_cores <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("invalid crew_launcher_pbs(): non-pbs field", {
  skip_if_low_dep_versions()
  x <- crew_launcher_pbs()
  x$name <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_pbs() script() nearly empty", {
  skip_if_low_dep_versions()
  x <- crew_launcher_pbs(
    pbs_cwd = FALSE,
    pbs_log_output = "log_file",
    pbs_log_join = FALSE,
    pbs_walltime_hours = NULL
  )
  expect_equal(
    x$script(name = "my_job"),
    c("#PBS -N my_job", "#PBS -o log_file", "#PBS -j n")
  )
})

test_that("crew_launcher_pbs() script() all lines", {
  skip_if_low_dep_versions()
  x <- crew_launcher_pbs(
    script_lines = c("module load R", "echo 'start'"),
    pbs_log_output = "out_dir/",
    pbs_log_error = "err_dir/",
    pbs_log_join = FALSE,
    pbs_memory_gigabytes_required = 2,
    pbs_cores = 2L,
    pbs_walltime_hours = 57
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
