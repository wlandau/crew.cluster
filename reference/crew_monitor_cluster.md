# **\[experimental\]** Create an abstract cluster monitor object.

Create an abstract cluster monitor `R6` object.

## Usage

``` r
crew_monitor_cluster(
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

Other cluster:
[`crew_class_launcher_cluster`](crew_class_launcher_cluster.md),
[`crew_class_monitor_cluster`](crew_class_monitor_cluster.md),
[`crew_launcher_cluster()`](crew_launcher_cluster.md),
[`crew_options_cluster()`](crew_options_cluster.md)
