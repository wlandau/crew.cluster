test_that("valid simple crew_launcher_slurm()", {
  expect_silent(crew_launcher_slurm())
})

test_that("valid populated crew_launcher_slurm()", {
  expect_silent(
    crew_launcher_slurm(
      script_lines = c("module load R", "echo 'start'"),
      slurm_log_output = "log1",
      slurm_log_error = "log2",
      slurm_memory_gigabytes_per_cpu = NULL,
      slurm_cpus_per_task = NULL
    )
  )
})

test_that("invalid crew_launcher_slurm(): SLURM field", {
  x <- crew_launcher_slurm()
  private <- crew_private(x)
  private$.slurm_cpus_per_task <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("invalid crew_launcher_slurm(): non-SLURM field", {
  skip_on_cran()
  x <- crew_launcher_slurm()
  x$set_name(- 1L)
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_slurm() script() nearly empty", {
  x <- crew_launcher_slurm(slurm_time_minutes = NULL)
  lines <- c(
    "#!/bin/sh",
    "#SBATCH --job-name=a_job",
    "#SBATCH --output=/dev/null",
    "#SBATCH --error=/dev/null"
  )
  expect_equal(x$script(name = "a_job"), lines)
})

test_that("crew_launcher_slurm() script() all lines", {
  x <- crew_launcher_slurm(
    script_lines = c("module load R", "echo 'start'"),
    slurm_log_output = "log1",
    slurm_log_error = "log2",
    slurm_memory_gigabytes_per_cpu = 2.096,
    slurm_cpus_per_task = 2,
    slurm_time_minutes = 57
  )
  out <- x$script(name = "this_job")
  exp <- c(
    "#!/bin/sh",
    "#SBATCH --job-name=this_job",
    "#SBATCH --output=log1",
    "#SBATCH --error=log2",
    "#SBATCH --mem-per-cpu=2146M",
    "#SBATCH --cpus-per-task=2",
    "#SBATCH --time=57",
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
  expect_warning(
    x <- crew_launcher_slurm(command_delete = "user_del"),
    class = "crew_deprecate"
  )
  expect_equal(x$command_terminate, "user_del")
})
