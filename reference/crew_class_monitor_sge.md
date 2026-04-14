# **\[experimental\]** SGE monitor class

SGE monitor `R6` class

## Details

See [`crew_monitor_sge()`](crew_monitor_sge.md).

## See also

Other sge: [`crew_class_launcher_sge`](crew_class_launcher_sge.md),
[`crew_controller_sge()`](crew_controller_sge.md),
[`crew_launcher_sge()`](crew_launcher_sge.md),
[`crew_monitor_sge()`](crew_monitor_sge.md),
[`crew_options_sge()`](crew_options_sge.md)

## Super class

[`crew.cluster::crew_class_monitor_cluster`](crew_class_monitor_cluster.md)
-\> `crew_class_monitor_sge`

## Methods

### Public methods

- [`crew_class_monitor_sge$jobs()`](#method-crew_class_monitor_sge-jobs)

- [`crew_class_monitor_sge$terminate()`](#method-crew_class_monitor_sge-terminate)

Inherited methods

- [`crew.cluster::crew_class_monitor_cluster$initialize()`](crew_class_monitor_cluster.html#method-initialize)
- [`crew.cluster::crew_class_monitor_cluster$validate()`](crew_class_monitor_cluster.html#method-validate)

------------------------------------------------------------------------

### Method `jobs()`

List SGE jobs.

#### Usage

    crew_class_monitor_sge$jobs(user = ps::ps_username())

#### Arguments

- `user`:

  Character of length 1, user name of the jobs to list.

#### Returns

A `tibble` with one row per SGE job and columns with specific details.

------------------------------------------------------------------------

### Method `terminate()`

Terminate one or more SGE jobs.

#### Usage

    crew_class_monitor_sge$terminate(jobs = NULL, all = FALSE)

#### Arguments

- `jobs`:

  Character vector of job names or job IDs to terminate. Ignored if
  `all` is set to `TRUE`.

- `all`:

  Logical of length 1, whether to terminate all the jobs under your user
  name. This terminates ALL your SGE jobs, regardless of whether
  `crew.cluster` launched them, so use with caution!

#### Returns

`NULL` (invisibly).
