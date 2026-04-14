# **\[experimental\]** SLURM options.

Set options for SLURM job management.

## Usage

``` r
crew_options_slurm(
  verbose = FALSE,
  command_submit = as.character(Sys.which("sbatch")),
  command_terminate = NULL,
  script_directory = tempdir(),
  script_lines = character(0L),
  log_output = "/dev/null",
  log_error = "/dev/null",
  memory_gigabytes_required = NULL,
  memory_gigabytes_per_cpu = NULL,
  cpus_per_task = NULL,
  time_minutes = NULL,
  partition = NULL,
  n_tasks = 1
)
```

## Arguments

- verbose:

  Logical, whether to see console output and error messages when
  submitting worker.

- command_submit:

  Character of length 1, file path to the executable to submit a worker
  job.

- command_terminate:

  Deprecated on 2025-08-26 in `crew.cluster` version 0.3.8.9001. No
  longer needed.

- script_directory:

  Character of length 1, directory path to the job scripts. Just before
  each job submission, a job script is created in this folder. Script
  base names are unique to each launcher and worker, and the launcher
  deletes the script when the worker is manually terminated.
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) is the default,
  but it might not work for some systems.
  `tools::R_user_dir("crew.cluster", which = "cache")` is another
  reasonable choice.

- script_lines:

  Optional character vector of additional lines to be added to the job
  script just after the more common flags. An example would be
  `script_lines = "module load R"` if your cluster supports R through an
  environment module.

- log_output:

  Character of length 1, file pattern to control the locations of the
  SLURM worker log files. By default, both standard output and standard
  error go to the same file. `log_output = "crew_log_%A.txt"` translates
  to a line of `#SBATCH --output=crew_log_%A.txt` in the SLURM job
  script, where `%A` is replaced by the job ID of the worker. The
  default is `/dev/null` to omit these logs. Set `log_output = NULL` to
  omit this line from the job script.

- log_error:

  Character of length 1, file pattern for standard error.
  `log_error = "crew_log_%A.txt"` translates to a line of
  `#SBATCH --error=crew_log_%A.txt` in the SLURM job script, where `%A`
  is replaced by the job ID of the worker. The default is `/dev/null` to
  omit these logs. Set `log_error = NULL` to omit this line from the job
  script.

- memory_gigabytes_required:

  Positive numeric scalar, total number of gigabytes of memory required
  per node. `memory_gigabytes_required = 2.40123` translates to a line
  of `#SBATCH --mem=2041M` in the SLURM job script.
  `memory_gigabytes_required = NULL` omits this line.

- memory_gigabytes_per_cpu:

  Positive numeric scalar, gigabytes of memory required per CPU.
  `memory_gigabytes_per_cpu = 2.40123` translates to a line of
  `#SBATCH --mem-per-cpu=2041M` in the SLURM job script.
  `memory_gigabytes_per_cpu = NULL` omits this line.

- cpus_per_task:

  Optional positive integer scalar, number of CPUs for the worker.
  `cpus_per_task = 4` translates to a line of
  `#SBATCH --cpus-per-task=4` in the SLURM job script.
  `cpus_per_task = NULL` omits this line.

- time_minutes:

  Numeric scalar, number of minutes to designate as the wall time of
  `crew` each worker instance on the SLURM cluster. `time_minutes = 60`
  translates to a line of `#SBATCH --time=60` in the SLURM job script.
  `time_minutes = NULL` omits this line.

- partition:

  Character string, name of the SLURM partition to create workers on.
  `partition = "partition1,partition2"` translates to a line of
  `#SBATCH --partition=partition1,partition2` in the SLURM job script.
  `partition = NULL` omits this line.

- n_tasks:

  Numeric scalar, number of SLURM tasks to run within the job.
  `n_tasks = 1` translates to a line of `#SBATCH --ntasks=1` in the
  SLURM job script. `n_tasks = 0` omits this line.

## Value

A classed list of options.

## Retryable options

Retryable options are deprecated in `crew.cluster` as of 2025-01-27
(version `0.3.4`).

## Attribution

The template files at
<https://github.com/mschubert/clustermq/tree/master/inst> informed the
development of the `crew` launcher plugins in `crew.cluster`, and we
would like to thank Michael Schubert for developing `clustermq` and
releasing it under the permissive Apache License 2.0. See the `NOTICE`
and `README.md` files in the `crew.cluster` source code for additional
attribution.

## See also

Other slurm:
[`crew_class_launcher_slurm`](crew_class_launcher_slurm.md),
[`crew_class_monitor_slurm`](crew_class_monitor_slurm.md),
[`crew_controller_slurm()`](crew_controller_slurm.md),
[`crew_launcher_slurm()`](crew_launcher_slurm.md),
[`crew_monitor_slurm()`](crew_monitor_slurm.md)

## Examples

``` r
  crew_options_slurm()
#> $verbose
#> [1] FALSE
#> 
#> $command_submit
#> [1] ""
#> 
#> $script_directory
#> [1] "/tmp/RtmppPc7SX"
#> 
#> $script_lines
#> character(0)
#> 
#> $log_output
#> [1] "/dev/null"
#> 
#> $log_error
#> [1] "/dev/null"
#> 
#> $memory_gigabytes_per_cpu
#> NULL
#> 
#> $memory_gigabytes_required
#> NULL
#> 
#> $cpus_per_task
#> NULL
#> 
#> $time_minutes
#> NULL
#> 
#> $partition
#> NULL
#> 
#> $n_tasks
#> [1] 1
#> 
#> attr(,"class")
#> [1] "crew_options_slurm"   "crew_options_cluster" "crew_options"        
```
