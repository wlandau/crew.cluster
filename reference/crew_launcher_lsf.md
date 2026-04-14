# **\[experimental\]** Create a launcher with LSF workers.

Create an `R6` object to launch and maintain workers as LSF jobs.

## Usage

``` r
crew_launcher_lsf(
  name = NULL,
  workers = 1L,
  seconds_interval = 0.5,
  seconds_timeout = 60,
  seconds_launch = 86400,
  seconds_idle = 300,
  seconds_wall = Inf,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = NULL,
  reset_packages = NULL,
  reset_options = NULL,
  garbage_collection = NULL,
  crashes_error = NULL,
  tls = crew::crew_tls(mode = "automatic"),
  r_arguments = c("--no-save", "--no-restore"),
  options_metrics = crew::crew_options_metrics(),
  options_cluster = crew.cluster::crew_options_lsf(),
  verbose = NULL,
  command_submit = NULL,
  command_terminate = NULL,
  command_delete = NULL,
  script_directory = NULL,
  script_lines = NULL,
  lsf_cwd = NULL,
  lsf_log_output = NULL,
  lsf_log_error = NULL,
  lsf_memory_gigabytes_limit = NULL,
  lsf_memory_gigabytes_required = NULL,
  lsf_cores = NULL
)
```

## Arguments

- name:

  Character string, name of the launcher. If the name is `NULL`, then a
  name is automatically generated when the launcher starts.

- workers:

  Maximum number of workers to run concurrently when auto-scaling,
  excluding task retries and manual calls to `launch()`. Special workers
  allocated for task retries do not count towards this limit, so the
  number of workers running at a given time may exceed this maximum. A
  smaller number of workers may run if the number of executing tasks is
  smaller than the supplied value of the `workers` argument.

- seconds_interval:

  Number of seconds between polling intervals waiting for certain
  internal synchronous operations to complete. In certain cases,
  exponential backoff is used with this argument passed to `seconds_max`
  in a
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.html)
  object.

- seconds_timeout:

  Number of seconds until timing out while waiting for certain
  synchronous operations to complete, such as checking
  [`mirai::info()`](https://mirai.r-lib.org/reference/info.html).

- seconds_launch:

  Seconds of startup time to allow. A worker is unconditionally assumed
  to be alive from the moment of its launch until `seconds_launch`
  seconds later. After `seconds_launch` seconds, the worker is only
  considered alive if it is actively connected to its assign websocket.

- seconds_idle:

  Maximum number of seconds that a worker can idle since the completion
  of the last task. If exceeded, the worker exits. But the timer does
  not launch until `tasks_timers` tasks have completed. See the
  `idletime` argument of
  [`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html).
  `crew` does not excel with perfectly transient workers because it does
  not micromanage the assignment of tasks to workers, so please allow
  enough idle time for a new worker to be delegated a new task.

- seconds_wall:

  Soft wall time in seconds. The timer does not launch until
  `tasks_timers` tasks have completed. See the `walltime` argument of
  [`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html).

- tasks_max:

  Maximum number of tasks that a worker will do before exiting. Also
  determines how often the controller auto-scales. See the Auto-scaling
  section for details.

- tasks_timers:

  Number of tasks to do before activating the timers for `seconds_idle`
  and `seconds_wall`. See the `timerstart` argument of
  [`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html).

- reset_globals:

  Deprecated on 2025-05-30 (`crew` version 1.1.2.9004). Please use the
  `reset_globals` option of
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.html)
  instead.

- reset_packages:

  Deprecated on 2025-05-30 (`crew` version 1.1.2.9004). Please use the
  `reset_packages` option of
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.html)
  instead.

- reset_options:

  Deprecated on 2025-05-30 (`crew` version 1.1.2.9004). Please use the
  `reset_options` option of
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.html)
  instead.

- garbage_collection:

  Deprecated on 2025-05-30 (`crew` version 1.1.2.9004). Please use the
  `garbage_collection` option of
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.html)
  instead.

- crashes_error:

  Deprecated on 2025-01-13 (`crew` version 0.10.2.9002).

- tls:

  A TLS configuration object from
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.html).

- r_arguments:

  Optional character vector of command line arguments to pass to
  `Rscript` (non-Windows) or `Rscript.exe` (Windows) when starting a
  worker. Example:
  `r_arguments = c("--vanilla", "--max-connections=32")`.

- options_metrics:

  Either `NULL` to opt out of resource metric logging for workers, or an
  object from
  [`crew_options_metrics()`](https://wlandau.github.io/crew/reference/crew_options_metrics.html)
  to enable and configure resource metric logging for workers. For
  resource logging to run, the `autometric` R package version 0.1.0 or
  higher must be installed.

- options_cluster:

  An options list from [`crew_options_lsf()`](crew_options_lsf.md) with
  cluster-specific configuration options.

- verbose:

  Deprecated. Use `options_cluster` instead.

- command_submit:

  Deprecated. Use `options_cluster` instead.

- command_terminate:

  Deprecated. Use `options_cluster` instead.

- command_delete:

  Deprecated on 2024-01-08 (version 0.1.4.9001). Use `command_terminate`
  instead.

- script_directory:

  Deprecated. Use `options_cluster` instead.

- script_lines:

  Deprecated. Use `options_cluster` instead.

- lsf_cwd:

  Deprecated. Use `options_cluster` instead.

- lsf_log_output:

  Deprecated. Use `options_cluster` instead.

- lsf_log_error:

  Deprecated. Use `options_cluster` instead.

- lsf_memory_gigabytes_limit:

  Deprecated. Use `options_cluster` instead.

- lsf_memory_gigabytes_required:

  Deprecated. Use `options_cluster` instead.

- lsf_cores:

  Deprecated. Use `options_cluster` instead.

## Details

WARNING: the `crew.cluster` LSF plugin is experimental. Please proceed
with caution and report bugs to
<https://github.com/wlandau/crew.cluster>.

To launch a LSF worker, this launcher creates a temporary job script
with a call to
[`crew::crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.html)
and submits it as an LSF job with `sbatch`. To see most of the lines of
the job script in advance, use the `script()` method of the launcher. It
has all the lines except for the job name and the call to
[`crew::crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.html),
both of which will be inserted at the last minute when it is time to
actually launch a worker.

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
[`crew_options_lsf()`](crew_options_lsf.md)
