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

# https://github.com/wlandau/crew/issues/217
test_that("crew_controller_sge() cleanup deprecations", {
  x <- crew_controller_sge()
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
