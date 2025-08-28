test_that("crew_controller_lsf() script() nearly empty", {
  x <- crew_controller_lsf(
    options_cluster = crew_options_lsf(cwd = "/home")
  )
  lines <- c(
    "#!/bin/sh",
    "#BSUB -J \"name[1-3]\"",
    "#BSUB -cwd /home",
    "#BSUB -o /dev/null",
    "#BSUB -e /dev/null"
  )
  expect_equal(x$launcher$script(name = "name", n = 3L), lines)
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
  out <- x$launcher$script(name = "my_name", n = 4L)
  exp <- c(
    "#!/bin/sh",
    "#BSUB -J \"my_name[1-4]\"",
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

# https://github.com/wlandau/crew/issues/217
test_that("crew_controller_lsf() cleanup deprecations", {
  x <- crew_controller_lsf()
  fields <- c(
    "reset_globals",
    "reset_packages",
    "reset_globals",
    "garbage_collection"
  )
  for (field in fields) {
    expect_true(is.logical(x[[field]]))
    expect_null(x$launcher[[field]])
  }
})
