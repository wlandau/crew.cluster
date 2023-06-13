test_that("valid simple crew_launcher_sge()", {
  skip_if_low_dep_versions()
  expect_silent(crew_launcher_sge())
})

test_that("valid populated crew_launcher_sge()", {
  skip_if_low_dep_versions()
  expect_silent(
    crew_launcher_sge(
      script_lines = c("module load R", "echo 'start'"),
      sge_cwd = TRUE,
      sge_envvars = TRUE,
      sge_log_output = "out",
      sge_log_error = "err",
      sge_log_join = FALSE,
      sge_memory_gigabytes_required = 2,
      sge_memory_gigabytes_limit = 8,
      sge_cores = 2L,
      sge_gpu = 1L
    )
  )
})

test_that("invalid crew_launcher_sge(): SGE field", {
  skip_if_low_dep_versions()
  x <- crew_launcher_sge()
  x$sge_cores <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("invalid crew_launcher_sge(): non-SGE field", {
  skip_if_low_dep_versions()
  x <- crew_launcher_sge()
  x$name <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_sge() script() nearly empty", {
  skip_if_low_dep_versions()
  x <- crew_launcher_sge(
    sge_cwd = FALSE,
    sge_envvars = FALSE,
    sge_log_output = "log_file",
    sge_log_join = FALSE
  )
  expect_equal(
    x$script(name = "my_job"),
    c("#$ -N my_job", "#$ -o log_file", "#$ -j n")
  )
})

test_that("crew_launcher_sge() script() all lines", {
  skip_if_low_dep_versions()
  x <- crew_launcher_sge(
    script_lines = c("module load R", "echo 'start'"),
    sge_cwd = TRUE,
    sge_envvars = TRUE,
    sge_log_output = "out_dir/",
    sge_log_error = "err_dir/",
    sge_log_join = FALSE,
    sge_memory_gigabytes_required = 2,
    sge_memory_gigabytes_limit = 8.4,
    sge_cores = 2L,
    sge_gpu = 1L
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
