test_that("crew_controller_sge() script() all lines", {
  x <- crew_controller_sge(
    options_cluster = crew_options_sge(
      script_lines = c("module load R", "echo 'start'"),
      cwd = TRUE,
      envvars = TRUE,
      log_output = "out_dir/",
      log_error = "err_dir/",
      log_join = FALSE,
      memory_gigabytes_required = 2.4,
      memory_gigabytes_limit = 8,
      cores = 2L,
      gpu = 1L
    )
  )
  out <- x$launcher$script(name = "a_job", attempt = 1L)
  exp <- c(
    "#$ -N a_job",
    "#$ -cwd",
    "#$ -V",
    "#$ -o out_dir/",
    "#$ -e err_dir/",
    "#$ -j n",
    "#$ -l h_rss=8G",
    "#$ -l m_mem_free=2.4G",
    "#$ -pe smp 2",
    "#$ -l gpu=1",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})

test_that("crew_controller_sge() retryable options in script", {
  x <- crew_controller_sge(
    options_cluster = crew_options_sge(
      script_lines = c("module load R", "echo 'start'"),
      cwd = TRUE,
      envvars = TRUE,
      log_output = "out_dir/",
      log_error = "err_dir/",
      log_join = FALSE,
      memory_gigabytes_required = c(2.4, 5.7),
      memory_gigabytes_limit = c(8, 3),
      cores = c(2L, 3L),
      gpu = c(1L, 2L)
    )
  )
  out <- x$launcher$script(name = "a_job", attempt = 1L)
  exp <- c(
    "#$ -N a_job",
    "#$ -cwd",
    "#$ -V",
    "#$ -o out_dir/",
    "#$ -e err_dir/",
    "#$ -j n",
    "#$ -l h_rss=8G",
    "#$ -l m_mem_free=2.4G",
    "#$ -pe smp 2",
    "#$ -l gpu=1",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
  out <- x$launcher$script(name = "a_job", attempt = 2L)
  exp <- c(
    "#$ -N a_job",
    "#$ -cwd",
    "#$ -V",
    "#$ -o out_dir/",
    "#$ -e err_dir/",
    "#$ -j n",
    "#$ -l h_rss=3G",
    "#$ -l m_mem_free=5.7G",
    "#$ -pe smp 3",
    "#$ -l gpu=2",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})
