test_that("valid simple crew_launcher_lsf()", {
  expect_no_error(crew_launcher_lsf())
})

test_that("valid populated crew_launcher_lsf()", {
  expect_no_error(
    crew_launcher_lsf(
      options_cluster = crew_options_lsf(
        script_lines = c("module load R", "echo 'start'"),
        cwd = "/home",
        log_output = "log1",
        log_error = "log2",
        memory_gigabytes_limit = NULL,
        memory_gigabytes_required = NULL,
        cores = NULL
      )
    )
  )
})

test_that("invalid crew_launcher_lsf(): lsf field", {
  x <- crew_launcher_lsf()
  private <- crew_private(x)
  private$.options_cluster$cores <- -1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_lsf() script() nearly empty", {
  x <- crew_launcher_lsf(
    options_cluster = crew_options_lsf(cwd = "/home")
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
  out <- x$script(name = "this_job")
  exp <- c(
    "#!/bin/sh",
    "#BSUB -J this_job",
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

test_that("crew_launcher_lsf() .args_launch()", {
  x <- crew_launcher_lsf()
  private <- crew_private(x)
  expect_equal(
    private$.args_launch(script = "this_script"),
    c("<", shQuote("this_script"))
  )
})

test_that("crew_launcher_lsf() .args_terminate()", {
  x <- crew_launcher_lsf()
  private <- crew_private(x)
  expect_equal(
    private$.args_terminate(name = "this_name"),
    c("-J", shQuote("this_name"))
  )
})

test_that("deprecate command_delete", {
  skip_on_cran()
  suppressWarnings(
    expect_warning(
      x <- crew_launcher_lsf(command_delete = "user_del"),
      class = "crew_deprecate"
    )
  )
  expect_equal(x$options_cluster$command_terminate, "user_del")
})
