test_that("valid simple crew_launcher_lsf()", {
  expect_silent(crew_launcher_lsf())
})

test_that("valid populated crew_launcher_lsf()", {
  expect_silent(
    crew_launcher_lsf(
      script_lines = c("module load R", "echo 'start'"),
      lsf_cwd = "/home",
      lsf_log_output = "log1",
      lsf_log_error = "log2",
      lsf_memory_megabytes_limit = NULL,
      lsf_cpus_per_task = NULL
    )
  )
})

test_that("invalid crew_launcher_lsf(): lsf field", {
  x <- crew_launcher_lsf()
  x$lsf_cpus_per_task <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("invalid crew_launcher_lsf(): non-lsf field", {
  x <- crew_launcher_lsf()
  x$name <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_lsf() script() nearly empty", {
  x <- crew_launcher_lsf(
    lsf_cwd = "/home"
  )
  lines <- c(
    "#!/bin/sh",
    "#BSUB -J a_job",
    "#BSUB -cwd /home",
    "#BSUB -o /dev/null",
    "#BSUB -e /dev/null"
  )
  expect_equal(x$script(name = "a_job"), lines)
})

test_that("crew_launcher_lsf() script() all lines", {
  x <- crew_launcher_lsf(
    script_lines = c("module load R", "echo 'start'"),
    lsf_cwd = "/home",
    lsf_log_output = "log1",
    lsf_log_error = "log2",
    lsf_memory_megabytes_limit = 2096,
    lsf_cpus_per_task = 2
  )
  out <- x$script(name = "this_job")
  exp <- c(
    "#!/bin/sh",
    "#BSUB -J this_job",
    "#BSUB -cwd /home",
    "#BSUB -o log1",
    "#BSUB -e log2",
    "#BSUB -M 2096MB",
    "#BSUB -n 2",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})
