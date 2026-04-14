# **\[experimental\]** SLURM monitor class

SLURM monitor `R6` class

## Details

See [`crew_monitor_slurm()`](crew_monitor_slurm.md).

## See also

Other slurm:
[`crew_class_launcher_slurm`](crew_class_launcher_slurm.md),
[`crew_controller_slurm()`](crew_controller_slurm.md),
[`crew_launcher_slurm()`](crew_launcher_slurm.md),
[`crew_monitor_slurm()`](crew_monitor_slurm.md),
[`crew_options_slurm()`](crew_options_slurm.md)

## Super class

[`crew.cluster::crew_class_monitor_cluster`](crew_class_monitor_cluster.md)
-\> `crew_class_monitor_slurm`

## Methods

### Public methods

- [`crew_class_monitor_slurm$jobs()`](#method-crew_class_monitor_slurm-jobs)

- [`crew_class_monitor_slurm$terminate()`](#method-crew_class_monitor_slurm-terminate)

Inherited methods

- [`crew.cluster::crew_class_monitor_cluster$initialize()`](crew_class_monitor_cluster.html#method-initialize)
- [`crew.cluster::crew_class_monitor_cluster$validate()`](crew_class_monitor_cluster.html#method-validate)

------------------------------------------------------------------------

### Method `jobs()`

List SLURM jobs.

#### Usage

    crew_class_monitor_slurm$jobs(user = ps::ps_username())

#### Arguments

- `user`:

  Character of length 1, user name of the jobs to list.

#### Details

This function loads the entire SLURM queue for all users, so it may take
several seconds to execute. It is intended for interactive use, and
should especially be avoided in scripts where it is called frequently.
It requires SLURM version 20.02 or higher, along with the YAML plugin.

#### Returns

A `tibble` with one row per SLURM job and columns with specific details.

------------------------------------------------------------------------

### Method `terminate()`

Terminate one or more SLURM jobs.

#### Usage

    crew_class_monitor_slurm$terminate(jobs = NULL, all = FALSE)

#### Arguments

- `jobs`:

  Character vector of job names or job IDs to terminate. Ignored if
  `all` is set to `TRUE`.

- `all`:

  Logical of length 1, whether to terminate all the jobs under your user
  name. This terminates ALL your SLURM jobs, regardless of whether
  `crew.cluster` launched them, so use with caution!

#### Returns

`NULL` (invisibly).
