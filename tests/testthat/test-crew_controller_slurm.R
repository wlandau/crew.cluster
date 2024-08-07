test_that("crew_controller_slurm() script() nearly empty", {
  x <- crew_controller_slurm(slurm_time_minutes = NULL)
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
    slurm_memory_gigabytes_required = 5.07,
    slurm_memory_gigabytes_per_cpu = 4.07,
    slurm_cpus_per_task = 2,
    slurm_time_minutes = 57
  )
  out <- x$launcher$script(name = "my_name")
  exp <- c(
    "#!/bin/sh",
    "#SBATCH --job-name=my_name",
    "#SBATCH --output=log1",
    "#SBATCH --error=log2",
    "#SBATCH --mem=5192M",
    "#SBATCH --mem-per-cpu=4168M",
    "#SBATCH --cpus-per-task=2",
    "#SBATCH --time=57",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})

test_that("deprecate seconds_exit", {
  expect_warning(
    x <- crew_controller_slurm(seconds_exit = 1),
    class = "crew_deprecate"
  )
})
