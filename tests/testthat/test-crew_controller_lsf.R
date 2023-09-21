test_that("crew_controller_lsf() script() nearly empty", {
  x <- crew_controller_lsf(
    lsf_cwd = "/home"
  )
  lines <- c(
    "#!/bin/sh",
    "#BSUB -J name",
    "#BSUB -cwd /home",
    "#BSUB -o /dev/null",
    "#BSUB -e /dev/null"
  )
  expect_equal(x$launcher$script(name = "name"), lines)
})

test_that("crew_controller_lsf() script() all lines", {
  x <- crew_controller_lsf(
    script_lines = c("module load R", "echo 'start'"),
    lsf_cwd = "/home",
    lsf_log_output = "log1",
    lsf_log_error = "log2",
    lsf_memory_gigabytes_limit = 2,
    lsf_memory_gigabytes_required = 2,
    lsf_cores = 2
  )
  out <- x$launcher$script(name = "my_name")
  exp <- c(
    "#!/bin/sh",
    "#BSUB -J my_name",
    "#BSUB -cwd /home",
    "#BSUB -o log1",
    "#BSUB -e log2",
    "#BSUB -M 2G",
    "#BSUB -R 'rusage[mem=2G]'",
    "#BSUB -n 2",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})

test_that("deprecate seconds_exit", {
  expect_warning(
    x <- crew_controller_lsf(seconds_exit = 1),
    class = "crew_deprecate"
  )
})
