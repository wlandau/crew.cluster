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
  x$slurm_cpus_per_task <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("invalid crew_launcher_slurm(): non-SLURM field", {
  skip("TODO: add back full validation when the next {crew} is released.")
  x <- crew_launcher_slurm()
  x$name <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_slurm() script() nearly empty", {
  x <- crew_launcher_slurm()
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
    slurm_cpus_per_task = 2
  )
  out <- x$script(name = "this_job")
  exp <- c(
    "#!/bin/sh",
    "#SBATCH --job-name=this_job",
    "#SBATCH --output=log1",
    "#SBATCH --error=log2",
    "#SBATCH --mem-per-cpu=2.096G",
    "#SBATCH --cpus-per-task=2",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})

test_that("args_terminate()", {
  x <- crew_launcher_slurm()
  expect_equal(
    x$args_terminate(name = "my_job"),
    c("--name", shQuote("my_job"))
  )
})
