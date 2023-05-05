test_that("crew_controller_slurm() script() nearly empty", {
  x <- crew_controller_slurm()
  lines <- c(
    "#!/bin/sh",
    "#SBATCH --job-name=name",
    "#SBATCH --output=/dev/null",
    "#SBATCH --error=/dev/null"
  )
  expect_equal(x$launcher$script(name = "name"), lines)
})

test_that("crew_controller_slurm() script() all lines", {
  x <- crew_controller_slurm(
    script_lines = c("module load R", "echo 'start'"),
    slurm_log_output = "log1",
    slurm_log_error = "log2",
    slurm_memory_gigabytes_per_cpu = 4.07,
    slurm_cpus_per_task = 2
  )
  out <- x$launcher$script(name = "my_name")
  exp <- c(
    "#!/bin/sh",
    "#SBATCH --job-name=my_name",
    "#SBATCH --output=log1",
    "#SBATCH --error=log2",
    "#SBATCH --mem-per-cpu=4.07G",
    "#SBATCH --cpus-per-task=2",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})
