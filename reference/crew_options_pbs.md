# **\[experimental\]** PBS options.

Set options for PBS job management.

## Usage

``` r
crew_options_pbs(
  verbose = FALSE,
  command_submit = as.character(Sys.which("qsub")),
  command_terminate = NULL,
  script_directory = tempdir(),
  script_lines = character(0L),
  cwd = TRUE,
  log_output = "/dev/null",
  log_error = NULL,
  log_join = TRUE,
  memory_gigabytes_required = NULL,
  cores = NULL,
  walltime_hours = 12
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

- cwd:

  Logical of length 1, whether to set the working directory of the
  worker to the working directory it was launched from. `cwd = TRUE` is
  translates to a line of `cd "$O_WORKDIR"` in the job script. This line
  is inserted after the content of `script_lines` to make sure the
  `#PBS` directives are above system commands. `cwd = FALSE` omits this
  line.

- log_output:

  Character of length 1, file or directory path to PBS worker log files
  for standard output. `log_output = "VALUE"` translates to a line of
  `#PBS -o VALUE` in the PBS job script. The default is `/dev/null` to
  omit the logs. If you do supply a non-`/dev/null` value, it is
  recommended to supply a directory path with a trailing slash so that
  each worker gets its own set of log files.

- log_error:

  Character of length 1, file or directory path to PBS worker log files
  for standard error. `log_error = "VALUE"` translates to a line of
  `#PBS -e VALUE` in the PBS job script. The default of `NULL` omits
  this line. If you do supply a non-`/dev/null` value, it is recommended
  to supply a directory path with a trailing slash so that each worker
  gets its own set of log files.

- log_join:

  Logical, whether to join the stdout and stderr log files together into
  one file. `log_join = TRUE` translates to a line of `#PBS -j oe` in
  the PBS job script, while `log_join = FALSE` is equivalent to
  `#PBS -j n`. If `log_join = TRUE`, then `log_error` should be `NULL`.

- memory_gigabytes_required:

  Optional positive numeric scalar, gigabytes of memory required to run
  the worker. `memory_gigabytes_required = 2.4` translates to a line of
  `#PBS -l mem=2.4gb` in the PBS job script.
  `memory_gigabytes_required = NULL` omits this line.

- cores:

  Optional positive integer scalar, number of cores for the worker
  ("slots" in PBS lingo). `cores = 4` translates to a line of
  `#PBS -l ppn=4` in the PBS job script. `cores = NULL` omits this line.

- walltime_hours:

  Numeric scalar, hours of wall time to request for the worker.
  `walltime_hours = 23` translates to a line of
  `#PBS -l walltime=23:00:00` in the job script. `walltime_hours = NULL`
  omits this line.

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

Other pbs: [`crew_class_launcher_pbs`](crew_class_launcher_pbs.md),
[`crew_controller_pbs()`](crew_controller_pbs.md),
[`crew_launcher_pbs()`](crew_launcher_pbs.md)

## Examples

``` r
  crew_options_pbs()
#> $verbose
#> [1] FALSE
#> 
#> $command_submit
#> [1] ""
#> 
#> $script_directory
#> [1] "/tmp/RtmpijRtTZ"
#> 
#> $script_lines
#> character(0)
#> 
#> $cwd
#> [1] TRUE
#> 
#> $log_output
#> [1] "/dev/null"
#> 
#> $log_error
#> NULL
#> 
#> $log_join
#> [1] TRUE
#> 
#> $memory_gigabytes_required
#> NULL
#> 
#> $cores
#> NULL
#> 
#> $walltime_hours
#> [1] 12
#> 
#> attr(,"class")
#> [1] "crew_options_pbs"     "crew_options_cluster" "crew_options"        
```
