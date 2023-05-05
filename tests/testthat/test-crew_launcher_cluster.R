test_that("valid abstract cluster launcher object", {
  expect_silent(crew_launcher_cluster())
})

test_that("bad field in cluster launcher object", {
  x <- crew_launcher_cluster()
  x$verbose <- 2L
  expect_error(x$validate(), class = "crew_error")
})

test_that("SGE subclass mock job creates a tempdir() job script", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_launcher_sge(
    command_submit = "cat",
    command_delete = "echo",
    script_lines = c("module load R", "echo 'start'"),
    sge_cwd = TRUE,
    sge_envvars = TRUE,
    sge_log_output = "out_dir/",
    sge_log_error = "err_dir/",
    sge_log_join = FALSE,
    sge_memory_gigabytes_required = 2.4,
    sge_memory_gigabytes_limit = 8.4,
    sge_cores = 2L,
    sge_gpu = 1L
  )
  x$start()
  expect_null(x$prefix)
  x$launch(index = 1L, socket = "my_socket")
  expect_false(is.null(x$prefix))
  script <- path_script(
    dir = tempdir(),
    prefix = x$prefix,
    launcher = x$name,
    worker = 1L
  )
  job <- name_job(
    launcher = x$name,
    worker = 1L,
    instance = x$workers$handle[[1]]$instance
  )
  expect_true(file.exists(script))
  out <- readLines(script)
  exp <- c(
    paste("#$ -N", job),
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
  x$terminate()
  expect_false(file.exists(script))
})

test_that("SGE subclass mock job creates a custom job script", {
  skip_on_cran()
  skip_on_os("windows")
  dir <- file.path(tempfile(), basename(tempfile()), basename(tempfile()))
  x <- crew_launcher_sge(
    command_submit = "cat",
    command_delete = "echo",
    script_directory = dir,
    script_lines = c("module load R", "echo 'start'"),
    sge_cwd = TRUE,
    sge_envvars = TRUE,
    sge_log_output = "out_dir/",
    sge_log_error = "err_dir/",
    sge_log_join = FALSE,
    sge_memory_gigabytes_required = 2.4,
    sge_memory_gigabytes_limit = 8.4,
    sge_cores = 2L,
    sge_gpu = 1L
  )
  x$start()
  expect_null(x$prefix)
  x$launch(index = 1L, socket = "my_socket")
  expect_false(is.null(x$prefix))
  script <- path_script(
    dir = dir,
    prefix = x$prefix,
    launcher = x$name,
    worker = 1L
  )
  job <- name_job(
    launcher = x$name,
    worker = 1L,
    instance = x$workers$handle[[1]]$instance
  )
  expect_true(file.exists(script))
  out <- readLines(script)
  exp <- c(
    paste("#$ -N", job),
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
  x$terminate()
  expect_false(file.exists(script))
})
