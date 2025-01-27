test_that("crew_controller_pbs() script() all lines", {
  x <- crew_controller_pbs(
    options_cluster = crew_options_pbs(
      script_lines = c("module load R", "echo 'start'"),
      cwd = TRUE,
      log_output = "out_dir/",
      log_error = "err_dir/",
      log_join = FALSE,
      memory_gigabytes_required = 2,
      cores = 2L,
      walltime_hours = 57
    )
  )
  out <- x$launcher$script(name = "this_job")
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
