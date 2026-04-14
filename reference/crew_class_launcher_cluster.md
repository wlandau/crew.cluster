# **\[maturing\]** Abstract cluster launcher class

`R6` class to help develop specific cluster launcher plugins.

## Details

See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

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
[`crew_class_monitor_cluster`](crew_class_monitor_cluster.md),
[`crew_launcher_cluster()`](crew_launcher_cluster.md),
[`crew_monitor_cluster()`](crew_monitor_cluster.md),
[`crew_options_cluster()`](crew_options_cluster.md)

## Super class

[`crew::crew_class_launcher`](https://wlandau.github.io/crew/reference/crew_class_launcher.html)
-\> `crew_class_launcher_cluster`

## Active bindings

- `options_cluster`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

## Methods

### Public methods

- [`crew_class_launcher_cluster$new()`](#method-crew_class_launcher_cluster-new)

- [`crew_class_launcher_cluster$validate()`](#method-crew_class_launcher_cluster-validate)

- [`crew_class_launcher_cluster$launch_workers()`](#method-crew_class_launcher_cluster-launch_workers)

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

------------------------------------------------------------------------

### Method `new()`

Abstract launcher constructor.

#### Usage

    crew_class_launcher_cluster$new(
      name = NULL,
      workers = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      tls = NULL,
      r_arguments = NULL,
      options_metrics = NULL,
      options_cluster = NULL
    )

#### Arguments

- `name`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `workers`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `seconds_interval`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `seconds_timeout`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `seconds_launch`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `seconds_idle`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `seconds_wall`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `tasks_max`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `tasks_timers`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `reset_globals`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `reset_packages`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `reset_options`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `garbage_collection`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `tls`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `r_arguments`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `options_metrics`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

- `options_cluster`:

  See [`crew_launcher_cluster()`](crew_launcher_cluster.md).

#### Returns

An abstract launcher object.

------------------------------------------------------------------------

### Method `validate()`

Validate the launcher.

#### Usage

    crew_class_launcher_cluster$validate()

#### Returns

`NULL` (invisibly). Throws an error if a field is invalid.

------------------------------------------------------------------------

### Method `launch_workers()`

Launch a job array

#### Usage

    crew_class_launcher_cluster$launch_workers(call, n)

#### Arguments

- `call`:

  Character string, a namespaced call to
  [`crew::crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.html)
  which will run in the worker and accept tasks.

- `n`:

  Positive integer of length 1, number of workers to launch in the
  current round of auto-scaling.

#### Details

The `call` argument is R code that will run to initiate the worker.

#### Returns

A handle object to allow the termination of the worker later on.
