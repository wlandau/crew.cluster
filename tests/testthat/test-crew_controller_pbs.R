test_that("crew_controller_pbs() script() all lines", {
  x <- crew_controller_pbs(
    script_lines = c("module load R", "echo 'start'"),
    pbs_cwd = TRUE,
    pbs_log_output = "out_dir/",
    pbs_log_error = "err_dir/",
    pbs_log_join = FALSE,
    pbs_memory_gigabytes_required = 2,
    pbs_cores = 2L,
    pbs_walltime_hours = 57
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

test_that("deprecate seconds_exit", {
  expect_warning(
    x <- crew_controller_pbs(seconds_exit = 1),
    class = "crew_deprecate"
  )
})
