# Common abstract cluster options.

Common abstract cluster options.

## Usage

``` r
crew_options_cluster(
  verbose = FALSE,
  command_submit = as.character(Sys.which("qsub")),
  command_terminate = NULL,
  script_directory = tempdir(),
  script_lines = character(0L)
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

## Value

A classed list of options.

## Attribution

The template files at
<https://github.com/mschubert/clustermq/tree/master/inst> informed the
development of the `crew` launcher plugins in `crew.cluster`, and we
would like to thank Michael Schubert for developing `clustermq` and
releasing it under the permissive Apache License 2.0. See the `NOTICE`
and `README.md` files in the `crew.cluster` source code for additional
attribution.

## See also

Other cluster:
[`crew_class_launcher_cluster`](crew_class_launcher_cluster.md),
[`crew_class_monitor_cluster`](crew_class_monitor_cluster.md),
[`crew_launcher_cluster()`](crew_launcher_cluster.md),
[`crew_monitor_cluster()`](crew_monitor_cluster.md)

## Examples

``` r
  crew_options_cluster()
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
#> attr(,"class")
#> [1] "crew_options_cluster" "crew_options"        
```
