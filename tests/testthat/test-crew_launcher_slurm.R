test_that("valid simple crew_launcher_slurm()", {
  expect_no_error(crew_launcher_slurm())
})

test_that("valid populated crew_launcher_slurm()", {
  expect_no_error(
    crew_launcher_slurm(
      options_cluster = crew_options_slurm(
        script_lines = c("module load R", "echo 'start'"),
        log_output = "log1",
        log_error = "log2",
        memory_gigabytes_per_cpu = NULL,
        cpus_per_task = NULL
      )
    )
  )
})

test_that("invalid crew_launcher_slurm(): SLURM field", {
  x <- crew_launcher_slurm()
  private <- crew_private(x)
  private$.options_cluster$cpus_per_task <- -1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_slurm() script() nearly empty", {
  x <- crew_launcher_slurm(
    options_cluster = crew_options_slurm(time_minutes = NULL)
  )
  lines <- c(
    "#!/bin/sh",
    "#SBATCH --job-name=a_job",
    "#SBATCH --output=/dev/null",
    "#SBATCH --error=/dev/null",
    "#SBATCH --ntasks=1"
  )
  expect_equal(x$script(name = "a_job"), lines)
})

test_that("crew_launcher_slurm() script() all lines", {
  x <- crew_launcher_slurm(
    options_cluster = crew_options_slurm(
      script_lines = c("module load R", "echo 'start'"),
      log_output = "log1",
      log_error = "log2",
      memory_gigabytes_required = 4.1,
      memory_gigabytes_per_cpu = 2.096,
      cpus_per_task = 2,
      time_minutes = 57
    )
  )
  out <- x$script(name = "this_job")
  exp <- c(
    "#!/bin/sh",
    "#SBATCH --job-name=this_job",
    "#SBATCH --output=log1",
    "#SBATCH --error=log2",
    "#SBATCH --mem=4198M",
    "#SBATCH --mem-per-cpu=2146M",
    "#SBATCH --cpus-per-task=2",
    "#SBATCH --time=57",
    "#SBATCH --ntasks=1",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})

test_that(".args_terminate()", {
  x <- crew_launcher_slurm()
  private <- crew_private(x)
  expect_equal(
    private$.args_terminate(name = "my_job"),
    c("--name", shQuote("my_job"))
  )
})

test_that("deprecate command_delete", {
  skip_on_cran()
  suppressWarnings(
    expect_warning(
      x <- crew_launcher_slurm(command_delete = "user_del"),
      class = "crew_deprecate"
    )
  )
  expect_equal(x$options_cluster$command_terminate, "user_del")
})
