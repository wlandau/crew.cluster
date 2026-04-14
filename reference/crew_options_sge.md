# **\[maturing\]** SGE options.

Set options for SGE job management.

## Usage

``` r
crew_options_sge(
  verbose = FALSE,
  command_submit = as.character(Sys.which("qsub")),
  command_terminate = NULL,
  script_directory = tempdir(),
  script_lines = character(0L),
  cwd = TRUE,
  envvars = FALSE,
  log_output = "/dev/null",
  log_error = NULL,
  log_join = TRUE,
  memory_gigabytes_limit = NULL,
  memory_gigabytes_required = NULL,
  cores = NULL,
  gpu = NULL
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

  Logical of length 1, whether to launch the worker from the current
  working directory (as opposed to the user home directory).
  `cwd = TRUE` translates to a line of `#$ -cwd` in the SGE job script.
  `cwd = FALSE` omits this line.

- envvars:

  Logical of length 1, whether to forward the environment variables of
  the current session to the SGE worker. `envvars = TRUE` translates to
  a line of `#$ -V` in the SGE job script. `envvars = FALSE` omits this
  line.

- log_output:

  Character of length 1, file or directory path to SGE worker log files
  for standard output. `log_output = "VALUE"` translates to a line of
  `#$ -o VALUE` in the SGE job script. The default is `/dev/null` to
  omit the logs. If you do supply a non-`/dev/null` value, it is
  recommended to supply a directory path with a trailing slash so that
  each worker gets its own set of log files.

- log_error:

  Character of length 1, file or directory path to SGE worker log files
  for standard error. `log_error = "VALUE"` translates to a line of
  `#$ -e VALUE` in the SGE job script. The default of `NULL` omits this
  line. If you do supply a non-`/dev/null` value, it is recommended to
  supply a directory path with a trailing slash so that each worker gets
  its own set of log files.

- log_join:

  Logical, whether to join the stdout and stderr log files together into
  one file. `log_join = TRUE` translates to a line of `#$ -j y` in the
  SGE job script, while `log_join = FALSE` is equivalent to `#$ -j n`.
  If `log_join = TRUE`, then `log_error` should be `NULL`.

- memory_gigabytes_limit:

  Optional numeric scalar, maximum number of gigabytes of memory a
  worker is allowed to consume. If the worker consumes more than this
  level of memory, then SGE will terminate it.
  `memory_gigabytes_limit = 5.7"` translates to a line of
  `"#$ -l h_rss=5.7G"` in the SGE job script.
  `memory_gigabytes_limit = NULL` omits this line.

- memory_gigabytes_required:

  Optional positive numeric scalar, gigabytes of memory required to run
  the worker. `memory_gigabytes_required = 2.4` translates to a line of
  `#$ -l m_mem_free=2.4G` in the SGE job script.
  `memory_gigabytes_required = NULL` omits this line.

- cores:

  Optional positive integer scalar, number of cores per worker ("slots"
  in SGE lingo). `cores = 4` translates to a line of `#$ -pe smp 4` in
  the SGE job script. `cores = NULL` omits this line.

- gpu:

  Optional integer scalar, number of GPUs to request for the worker.
  `gpu = 1` translates to a line of `"#$ -l gpu=1"` in the SGE job
  script. `gpu = NULL` omits this line.

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

Other sge: [`crew_class_launcher_sge`](crew_class_launcher_sge.md),
[`crew_class_monitor_sge`](crew_class_monitor_sge.md),
[`crew_controller_sge()`](crew_controller_sge.md),
[`crew_launcher_sge()`](crew_launcher_sge.md),
[`crew_monitor_sge()`](crew_monitor_sge.md)

## Examples

``` r
  crew_options_sge()
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
#> $cwd
#> [1] TRUE
#> 
#> $envvars
#> [1] FALSE
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
#> $memory_gigabytes_limit
#> NULL
#> 
#> $memory_gigabytes_required
#> NULL
#> 
#> $cores
#> NULL
#> 
#> $gpu
#> NULL
#> 
#> attr(,"class")
#> [1] "crew_options_sge"     "crew_options_cluster" "crew_options"        
```
