# **\[experimental\]** LSF options.

Set options for LSF job management.

## Usage

``` r
crew_options_lsf(
  verbose = FALSE,
  command_submit = as.character(Sys.which("bsub")),
  command_terminate = NULL,
  script_directory = tempdir(),
  script_lines = character(0L),
  cwd = getwd(),
  log_output = "/dev/null",
  log_error = "/dev/null",
  memory_gigabytes_limit = NULL,
  memory_gigabytes_required = NULL,
  cores = NULL
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

  Character of length 1, directory to launch the worker from (as opposed
  to the system default). `cwd = "/home"` translates to a line of
  `#BSUB -cwd /home` in the LSF job script. `cwd = getwd()` is the
  default, which launches workers from the current working directory.
  Set `cwd = NULL` to omit this line from the job script.

- log_output:

  Character of length 1, file pattern to control the locations of the
  LSF worker log files. By default, both standard output and standard
  error go to the same file. `log_output = "crew_log_%J.log"` translates
  to a line of `#BSUB -o crew_log_%J.log` in the LSF job script, where
  `%J` is replaced by the job ID of the worker. The default is
  `/dev/null` to omit these logs. Set `log_output = NULL` to omit this
  line from the job script.

- log_error:

  Character of length 1, file pattern for standard error.
  `log_error = "crew_error_%J.err"` translates to a line of
  `#BSUB -e crew_error_%J.err` in the LSF job script, where `%J` is
  replaced by the job ID of the worker. The default is `/dev/null` to
  omit these logs. Set `log_error = NULL` to omit this line from the job
  script.

- memory_gigabytes_limit:

  Positive numeric scalar, memory limit in gigabytes of the worker.
  `memory_gigabytes_limit = 4` translates to a line of `#BSUB -M 4G` in
  the LSF job script. `memory_gigabytes_limit = NULL` omits this line.

- memory_gigabytes_required:

  Positive numeric scalar, memory requirement in gigabytes.
  `memory_gigabytes_required = 4` translates to a line of
  `#BSUB -R 'rusage[mem=4G]'` in the LSF job script.
  `memory_gigabytes_required = NULL` omits this line.

- cores:

  Optional positive integer scalar, number of CPU cores for the worker.
  `cores = 4` translates to a line of `#BSUB -n 4` in the LSF job
  script. `cores = NULL` omits this line.

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

Other lsf: [`crew_class_launcher_lsf`](crew_class_launcher_lsf.md),
[`crew_controller_lsf()`](crew_controller_lsf.md),
[`crew_launcher_lsf()`](crew_launcher_lsf.md)

## Examples

``` r
  crew_options_lsf()
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
#> [1] "/home/runner/work/crew.cluster/crew.cluster/docs/reference"
#> 
#> $log_output
#> [1] "/dev/null"
#> 
#> $log_error
#> [1] "/dev/null"
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
#> attr(,"class")
#> [1] "crew_options_lsf"     "crew_options_cluster" "crew_options"        
```
