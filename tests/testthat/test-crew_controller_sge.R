test_that("crew_controller_sge() script() all lines", {
  x <- crew_controller_sge(
    script_lines = c("module load R", "echo 'start'"),
    sge_cwd = TRUE,
    sge_envvars = TRUE,
    sge_log_output = "out_dir/",
    sge_log_error = "err_dir/",
    sge_log_join = FALSE,
    sge_memory_gigabytes_required = 2.4,
    sge_memory_gigabytes_limit = 8,
    sge_cores = 2L,
    sge_gpu = 1L
  )
  out <- x$launcher$script(name = "a_job")
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

test_that("deprecate seconds_exit", {
  expect_warning(
    x <- crew_controller_sge(seconds_exit = 1),
    class = "crew_deprecate"
  )
})
