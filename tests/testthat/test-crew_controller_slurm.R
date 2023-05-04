test_that("crew_controller_slurm() script() nearly empty", {
  x <- crew_controller_slurm()
  lines <- c(
    "#!/bin/sh",
    "$SBATCH --output=/dev/null",
    "$SBATCH --error=/dev/null"
  )
  expect_equal(x$launcher$script(), lines)
})

test_that("crew_controller_slurm() script() all lines", {
  x <- crew_controller_slurm(
    slurm_log_output = "log1",
    slurm_log_error = "log2",
    slurm_memory_megabytes_per_cpu = 2096,
    slurm_cpus_per_task = 2,
    slurm_lines = c("module load R", "echo 'start'")
  )
  out <- x$launcher$script()
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
