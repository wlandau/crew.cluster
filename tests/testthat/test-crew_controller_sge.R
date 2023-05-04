test_that("crew_controller_sge() script() all lines", {
  x <- crew_controller_sge(
    sge_cwd = TRUE,
    sge_envvars = TRUE,
    sge_log_files = "log_dir/",
    sge_log_join = TRUE,
    sge_memory_gigabytes_required = 2.4,
    sge_memory_gigabytes_limit = 8,
    sge_cores = 2L,
    sge_gpu = 1L,
    sge_lines = c("module load R", "echo 'start'")
  )
  out <- x$launcher$script()
  exp <- c(
    "#$ -cwd",
    "#$ -V",
    "#$ -o log_dir/",
    "#$ -j y",
    "$# -l m_mem_free=2.4G",
    "$# -l h_rss=8G",
    "$# -pe smp 2",
    "$# -l gpu=1",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})
