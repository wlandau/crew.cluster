test_that("crew_controller_lsf() script() nearly empty", {
  x <- crew_controller_lsf(
    options_cluster = crew_options_lsf(cwd = "/home")
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
    options_cluster = crew_options_lsf(
      script_lines = c("module load R", "echo 'start'"),
      cwd = "/home",
      log_output = "log1",
      log_error = "log2",
      memory_gigabytes_limit = 2,
      memory_gigabytes_required = 2,
      cores = 2
    )
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
