test_that("valid abstract cluster launcher object", {
  expect_no_error(crew_launcher_cluster())
})

test_that("bad field in cluster launcher object", {
  skip_on_cran()
  x <- crew_launcher_cluster()
  private <- crew_private(x)
  private$.options_cluster$verbose <- 2L
  expect_error(x$validate(), class = "crew_error")
})

test_that("active bindings", {
  x <- crew_launcher_cluster()
  expect_s3_class(x$options_cluster, c("crew_options_cluster", "crew_options"))
})

test_that("SGE subclass mock job creates a tempdir() job script", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("crew", minimum_version = "1.0.0")
  x <- crew_launcher_sge(
    options_cluster = crew_options_sge(
      command_submit = "cat",
      script_lines = c("module load R", "echo 'start'"),
      cwd = TRUE,
      envvars = TRUE,
      log_output = "out_dir/",
      log_error = "err_dir/",
      log_join = FALSE,
      memory_gigabytes_required = 2.4,
      memory_gigabytes_limit = 8.4,
      cores = 2L,
      gpu = 1L
    )
  )
  x$start(url = "my_url", profile = "my_profile")
  handle <- x$launch_workers(call = x$call(), n = 4L)
  expect_true(is.character(handle$script) && length(handle$script) == 1L)
  expect_false(anyNA(handle$script))
  expect_true(file.exists(handle$script))
  out <- readLines(handle$script)
  exp <- c(
    "#$ -t 1-4",
    "#$ -cwd",
    "#$ -V",
    "#$ -o out_dir/",
    "#$ -e err_dir/",
    "#$ -j n",
    "#$ -l h_rss=8.4G",
    "#$ -l m_mem_free=2.4G",
    "#$ -pe smp 2",
    "#$ -l gpu=1",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out[seq_along(exp) + 1], exp)
})

test_that("SGE subclass mock job creates a custom job script", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("crew", minimum_version = "1.0.0")
  dir <- file.path(tempfile(), basename(tempfile()), basename(tempfile()))
  x <- crew_launcher_sge(
    options_cluster = crew_options_sge(
      command_submit = "cat",
      script_directory = dir,
      script_lines = c("module load R", "echo 'start'"),
      cwd = TRUE,
      envvars = TRUE,
      log_output = "out_dir/",
      log_error = "err_dir/",
      log_join = FALSE,
      memory_gigabytes_required = 2.4,
      memory_gigabytes_limit = 8.4,
      cores = 2L,
      gpu = 1L
    )
  )
  private <- crew_private(x)
  x$start(url = "my_url", profile = "my_profile")
  handle <- x$launch_workers(call = x$call(), n = 8L)
  expect_true(is.character(handle$script) && length(handle$script) == 1L)
  expect_false(anyNA(handle$script))
  expect_true(file.exists(handle$script))
  expect_true(file.exists(handle$script))
  out <- readLines(handle$script)
  exp <- c(
    "#$ -t 1-8",
    "#$ -cwd",
    "#$ -V",
    "#$ -o out_dir/",
    "#$ -e err_dir/",
    "#$ -j n",
    "#$ -l h_rss=8.4G",
    "#$ -l m_mem_free=2.4G",
    "#$ -pe smp 2",
    "#$ -l gpu=1",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out[seq_along(exp) + 1L], exp)
})
