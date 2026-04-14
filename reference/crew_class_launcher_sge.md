# **\[maturing\]** SGE launcher class

`R6` class to launch and manage SGE workers.

## Details

See [`crew_launcher_sge()`](crew_launcher_sge.md).

## Attribution

The template files at
<https://github.com/mschubert/clustermq/tree/master/inst> informed the
development of the `crew` launcher plugins in `crew.cluster`, and we
would like to thank Michael Schubert for developing `clustermq` and
releasing it under the permissive Apache License 2.0. See the `NOTICE`
and `README.md` files in the `crew.cluster` source code for additional
attribution.

## See also

Other sge: [`crew_class_monitor_sge`](crew_class_monitor_sge.md),
[`crew_controller_sge()`](crew_controller_sge.md),
[`crew_launcher_sge()`](crew_launcher_sge.md),
[`crew_monitor_sge()`](crew_monitor_sge.md),
[`crew_options_sge()`](crew_options_sge.md)

## Super classes

[`crew::crew_class_launcher`](https://wlandau.github.io/crew/reference/crew_class_launcher.html)
-\>
[`crew.cluster::crew_class_launcher_cluster`](crew_class_launcher_cluster.md)
-\> `crew_class_launcher_sge`

## Methods

### Public methods

- [`crew_class_launcher_sge$validate()`](#method-crew_class_launcher_sge-validate)

- [`crew_class_launcher_sge$script()`](#method-crew_class_launcher_sge-script)

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

    crew_class_launcher_sge$validate()

#### Returns

`NULL` (invisibly). Throws an error if a field is invalid.

------------------------------------------------------------------------

### Method `script()`

Generate the job script.

#### Usage

    crew_class_launcher_sge$script(name, n)

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
    launcher <- crew_launcher_sge(
      sge_cores = 2,
      sge_memory_gigabytes_required = 4
    )
    launcher$script(name = "my_job_name")
    }

## Examples

``` r
## ------------------------------------------------
## Method `crew_class_launcher_sge$script`
## ------------------------------------------------

if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
launcher <- crew_launcher_sge(
  sge_cores = 2,
  sge_memory_gigabytes_required = 4
)
launcher$script(name = "my_job_name")
}
```
