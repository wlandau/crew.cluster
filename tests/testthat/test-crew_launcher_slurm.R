test_that("valid simple crew_launcher_slurm()", {
  expect_silent(crew_launcher_slurm())
})

test_that("valid populated crew_launcher_slurm()", {
  expect_silent(
    crew_launcher_slurm(
      slurm_log_output = "log1",
      slurm_log_error = "log2",
      slurm_memory_megabytes_per_cpu = NULL,
      slurm_cpus_per_task = NULL,
      slurm_lines = c("module load R", "echo 'start'")
    )
  )
})

test_that("invalid crew_launcher_slurm(): SLURM field", {
  x <- crew_launcher_slurm()
  x$slurm_cpus_per_task <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("invalid crew_launcher_slurm(): non-SLURM field", {
  x <- crew_launcher_slurm()
  x$name <- - 1L
  expect_error(x$validate(), class = "crew_error")
})

test_that("crew_launcher_slurm() script() nearly empty", {
  x <- crew_launcher_slurm()
  lines <- c(
    "#!/bin/sh",
    "$SBATCH --output=/dev/null",
    "$SBATCH --error=/dev/null"
  )
  expect_equal(x$script(), lines)
})

test_that("crew_launcher_slurm() script() all lines", {
  x <- crew_launcher_slurm(
    slurm_log_output = "log1",
    slurm_log_error = "log2",
    slurm_memory_megabytes_per_cpu = 2096,
    slurm_cpus_per_task = 2,
    slurm_lines = c("module load R", "echo 'start'")
  )
  out <- x$script()
  exp <- c(
    "#!/bin/sh",
    "$SBATCH --output=log1",
    "$SBATCH --error=log2",
    "$SBATCH --mem-per-cpu=2096",
    "$SBATCH --cpus-per-task=2",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})
