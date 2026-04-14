# **\[experimental\]** Create a SLURM monitor object.

Create an `R6` object to monitor SLURM cluster jobs.

## Usage

``` r
crew_monitor_slurm(
  verbose = TRUE,
  command_list = as.character(Sys.which("squeue")),
  command_terminate = as.character(Sys.which("scancel"))
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

Other slurm:
[`crew_class_launcher_slurm`](crew_class_launcher_slurm.md),
[`crew_class_monitor_slurm`](crew_class_monitor_slurm.md),
[`crew_controller_slurm()`](crew_controller_slurm.md),
[`crew_launcher_slurm()`](crew_launcher_slurm.md),
[`crew_options_slurm()`](crew_options_slurm.md)
