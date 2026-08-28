# **\[experimental\]** SLURM launcher class

`R6` class to launch and manage SLURM workers.

## Details

See [`crew_launcher_slurm()`](crew_launcher_slurm.md).

## Attribution

The template files at
<https://github.com/mschubert/clustermq/tree/master/inst> informed the
development of the `crew` launcher plugins in `crew.cluster`, and we
would like to thank Michael Schubert for developing `clustermq` and
releasing it under the permissive Apache License 2.0. See the `NOTICE`
and `README.md` files in the `crew.cluster` source code for additional
attribution.

## See also

Other slurm: [`crew_class_monitor_slurm`](crew_class_monitor_slurm.md),
[`crew_controller_slurm()`](crew_controller_slurm.md),
[`crew_launcher_slurm()`](crew_launcher_slurm.md),
[`crew_monitor_slurm()`](crew_monitor_slurm.md),
[`crew_options_slurm()`](crew_options_slurm.md)

## Super classes

[`crew::crew_class_launcher`](https://wlandau.github.io/crew/reference/crew_class_launcher.html)
-\>
[`crew.cluster::crew_class_launcher_cluster`](crew_class_launcher_cluster.md)
-\> `crew_class_launcher_slurm`

## Methods

### Public methods

- [`crew_class_launcher_slurm$validate()`](#method-crew_class_launcher_slurm-validate)

- [`crew_class_launcher_slurm$script()`](#method-crew_class_launcher_slurm-script)

Inherited methods

- [`crew::crew_class_launcher$call()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-call)
- [`crew::crew_class_launcher$crashes()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-crashes)
- [`crew::crew_class_launcher$launch()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-launch)
- [`crew::crew_class_launcher$launch_worker()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-launch_worker)
- [`crew::crew_class_launcher$poll()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-poll)
- [`crew::crew_class_launcher$scale()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-scale)
- [`crew::crew_class_launcher$set_name()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-set_name)
- [`crew::crew_class_launcher$settings()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-settings)
- [`crew::crew_class_launcher$start()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-start)
- [`crew::crew_class_launcher$terminate()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-terminate)
- [`crew::crew_class_launcher$terminate_workers()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-terminate_workers)
- [`crew.cluster::crew_class_launcher_cluster$initialize()`](crew_class_launcher_cluster.html#method-initialize)
- [`crew.cluster::crew_class_launcher_cluster$launch_workers()`](crew_class_launcher_cluster.html#method-launch_workers)

------------------------------------------------------------------------

### Method `validate()`

Validate the launcher.

#### Usage

    crew_class_launcher_slurm$validate()

#### Returns

`NULL` (invisibly). Throws an error if a field is invalid.

------------------------------------------------------------------------

### Method `script()`

Generate the job script.

#### Usage

    crew_class_launcher_slurm$script(name, n)

#### Arguments

- `name`:

  Character of length 1, name of the job. For inspection purposes, you
  can supply a mock job name.

- `n`:

  Positive integer of length 1, number of crew workers (i.e. cluster
  jobs) to launch in the current round of auto-scaling.

#### Details

Includes everything except the worker-instance-specific job name and the
worker-instance-specific call to
[`crew::crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.html),
both of which get inserted at the bottom of the script at launch time.

#### Returns

Character vector of the lines of the job script.

#### Examples

    if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    launcher <- crew_launcher_slurm(
      slurm_log_output = "log_file_%A.log",
      slurm_log_error = NULL,
      slurm_memory_gigabytes_per_cpu = 4096
    )
    launcher$script(name = "my_job_name")
    }

## Examples

``` r

## ------------------------------------------------
## Method `crew_class_launcher_slurm$script`
## ------------------------------------------------

if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
launcher <- crew_launcher_slurm(
  slurm_log_output = "log_file_%A.log",
  slurm_log_error = NULL,
  slurm_memory_gigabytes_per_cpu = 4096
)
launcher$script(name = "my_job_name")
}
```
