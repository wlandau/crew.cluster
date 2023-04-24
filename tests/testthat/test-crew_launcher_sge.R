test_that("valid simple crew_launcher_sge()", {
  expect_silent(crew_launcher_sge())
})

test_that("valid populated crew_launcher_sge()", {
  expect_silent(
    crew_launcher_sge(
      sge_cwd = TRUE,
      sge_envvars = TRUE,
      sge_log_files = "log",
      sge_log_join = TRUE,
      sge_memory_gigabytes_required = 2,
      sge_memory_gigabytes_limit = 8,
      sge_cores = 2L,
      sge_gpu = 1L,
      sge_lines = c("module load R", "echo 'start'")
    )
  )
})

test_that("invalid crew_launcher_sge(): SGE field", {
  x <- crew_launcher_sge()
  x$sge_cores <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("invalid crew_launcher_sge(): nonSGE field", {
  x <- crew_launcher_sge()
  x$name <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_sge() script() nearly empty", {
  x <- crew_launcher_sge(
    sge_cwd = FALSE,
    sge_envvars = FALSE,
    sge_log_files = "log_file",
    sge_log_join = FALSE
  )
  expect_equal(x$script(), c("#$ -o log_file", "#$ -j n"))
})

test_that("crew_launcher_sge() script() all lines", {
  x <- crew_launcher_sge(
    sge_cwd = TRUE,
    sge_envvars = TRUE,
    sge_log_files = "log_dir/",
    sge_log_join = TRUE,
    sge_memory_gigabytes_required = 2,
    sge_memory_gigabytes_limit = 8.4,
    sge_cores = 2L,
    sge_gpu = 1L,
    sge_lines = c("module load R", "echo 'start'")
  )
  out <- x$script()
  exp <- c(
    "#$ -cwd",
    "#$ -V",
    "#$ -o log_dir/",
    "#$ -j y",
    "$# -l m_mem_free=2G",
    "$# -l h_rss=8.4G",
    "$# -pe smp 2",
    "$# -l gpu=1",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})
