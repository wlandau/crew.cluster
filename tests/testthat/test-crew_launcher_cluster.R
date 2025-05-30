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
      command_terminate = "echo",
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
  private <- crew_private(x)
  expect_null(private$.prefix)
  handle <- x$launch_worker(
    call = x$call(worker = "worker_name"),
    name = "my_name",
    launcher = x$name,
    worker = "worker_name"
  )
  expect_false(is.null(private$.prefix))
  script <- path_script(
    dir = tempdir(),
    prefix = private$.prefix,
    launcher = x$name,
    worker = "worker_name"
  )
  expect_equal(handle$name, "my_name")
  expect_equal(handle$script, script)
  expect_true(file.exists(script))
  out <- readLines(script)
  exp <- c(
    "#$ -N my_name",
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
  expect_equal(out[seq_along(exp)], exp)
  x$terminate_worker(handle)
  expect_false(file.exists(script))
})

test_that("SGE subclass mock job creates a custom job script", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("crew", minimum_version = "1.0.0")
  dir <- file.path(tempfile(), basename(tempfile()), basename(tempfile()))
  x <- crew_launcher_sge(
    options_cluster = crew_options_sge(
      command_submit = "cat",
      command_terminate = "echo",
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
  expect_null(private$.prefix)
  handle <- x$launch_worker(
    call = x$call(worker = "my_worker"),
    name = "my_name",
    launcher = x$name,
    worker = "my_worker"
  )
  expect_false(is.null(private$.prefix))
  script <- path_script(
    dir = dir,
    prefix = private$.prefix,
    launcher = x$name,
    worker = "my_worker"
  )
  expect_equal(handle$name, "my_name")
  expect_equal(handle$script, script)
  expect_true(file.exists(script))
  out <- readLines(script)
  exp <- c(
    "#$ -N my_name",
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
  expect_equal(out[seq_along(exp)], exp)
  x$terminate_worker(handle)
  expect_false(file.exists(script))
})
