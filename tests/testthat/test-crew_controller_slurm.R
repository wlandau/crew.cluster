test_that("crew_controller_slurm() script() nearly empty", {
  x <- crew_controller_slurm()
  lines <- c(
    "#!/bin/sh",
    "#SBATCH --job-name=name",
    "#SBATCH --output=/dev/null",
    "#SBATCH --error=/dev/null",
    "#SBATCH --ntasks=1"
  )
  expect_equal(x$launcher$script(name = "name"), lines)
})

test_that("crew_controller_slurm() script() without --ntasks", {
  x <- crew_controller_slurm(
    options_cluster = crew_options_slurm(n_tasks = NULL)
  )
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
    options_cluster = crew_options_slurm(
      script_lines = c("module load R", "echo 'start'"),
      log_output = "log1",
      log_error = "log2",
      memory_gigabytes_required = 5.07,
      memory_gigabytes_per_cpu = 4.07,
      cpus_per_task = 2,
      time_minutes = 57,
      n_tasks = 3
    )
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
    "#SBATCH --ntasks=3",
    "module load R",
    "echo 'start'"
  )
  expect_equal(out, exp)
})

# https://github.com/wlandau/crew/issues/217
test_that("crew_controller_slurm() cleanup deprecations", {
  x <- crew_controller_slurm()
  fields <- c(
    "reset_globals",
    "reset_packages",
    "reset_globals",
    "garbage_collection"
  )
  for (field in fields) {
    expect_true(is.logical(x[[field]]))
    expect_null(x$launcher[[field]])
  }
})
