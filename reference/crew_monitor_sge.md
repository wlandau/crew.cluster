# **\[experimental\]** Create a SGE monitor object.

Create an `R6` object to monitor SGE cluster jobs.

## Usage

``` r
crew_monitor_sge(
  verbose = TRUE,
  command_list = as.character(Sys.which("qstat")),
  command_terminate = as.character(Sys.which("qdel"))
)
```

## Arguments

- verbose:

  Deprecated. Use `options_cluster` instead.

- command_list:

  Character of length 1, file path to the executable to list jobs.

- command_terminate:

  Deprecated. Use `options_cluster` instead.

## See also

Other sge: [`crew_class_launcher_sge`](crew_class_launcher_sge.md),
[`crew_class_monitor_sge`](crew_class_monitor_sge.md),
[`crew_controller_sge()`](crew_controller_sge.md),
[`crew_launcher_sge()`](crew_launcher_sge.md),
[`crew_options_sge()`](crew_options_sge.md)
