test_that("valid simple crew_launcher_sge()", {
  expect_silent(crew_launcher_sge())
})

test_that("valid populated crew_launcher_sge()", {
  expect_silent(
    crew_launcher_sge(
      options_cluster = crew_options_sge(
        script_lines = c("module load R", "echo 'start'"),
        cwd = TRUE,
        envvars = TRUE,
        log_output = "out",
        log_error = "err",
        log_join = FALSE,
        memory_gigabytes_required = 2,
        memory_gigabytes_limit = 8,
        cores = 2L,
        gpu = 1L
      )
    )
  )
})

test_that("invalid crew_launcher_sge(): SGE field", {
  x <- crew_launcher_sge()
  private <- crew_private(x)
  private$.options_cluster$cores <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_sge() script() nearly empty", {
  x <- crew_launcher_sge(
    options_cluster = crew_options_sge(
      cwd = FALSE,
      envvars = FALSE,
      log_output = "log_file",
      log_join = FALSE
    )
  )
  expect_equal(
    x$script(name = "my_job"),
    c("#$ -N my_job", "#$ -o log_file", "#$ -j n")
  )
})

test_that("crew_launcher_sge() script() all lines", {
  x <- crew_launcher_sge(
    options_cluster = crew_options_sge(
      script_lines = c("module load R", "echo 'start'"),
      cwd = TRUE,
      envvars = TRUE,
      log_output = "out_dir/",
      log_error = "err_dir/",
      log_join = FALSE,
      memory_gigabytes_required = 2,
      memory_gigabytes_limit = 8.4,
      cores = 2L,
      gpu = 1L
    )
  )
  out <- x$script(name = "this_job")
  exp <- c(
    "#$ -N this_job",
    "#$ -cwd",
    "#$ -V",
    "#$ -o out_dir/",
    "#$ -e err_dir/",
    "#$ -j n",
    "#$ -l h_rss=8.4G",
    "#$ -l m_mem_free=2G",
    "#$ -pe smp 2",
    "#$ -l gpu=1",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})

test_that("deprecate command_delete", {
  skip_on_cran()
  suppressWarnings(
    expect_warning(
      x <- crew_launcher_sge(command_delete = "user_del"),
      class = "crew_deprecate"
    )
  )
  expect_equal(x$options_cluster$command_terminate, "user_del")
})
