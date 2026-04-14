# Changelog

## crew.cluster 0.4.0.9000 (development)

## crew.cluster 0.4.0

CRAN release: 2025-09-15

- Remove `termiante_worker()` and deprecate `command_terminate`
  (<https://github.com/wlandau/crew/pull/236>).
- Only use `call` and `name` in `launch_worker()`.
- Support job arrays
  ([\#56](https://github.com/wlandau/crew.cluster/issues/56),
  [\#57](https://github.com/wlandau/crew.cluster/issues/57)).
- Allow custom compute profiles.

## crew.cluster 0.3.8

CRAN release: 2025-06-09

- Use `expect_no_error()` instead of `expect_silent()` to ensure
  compatibility with the deprecation cycle to fix
  <https://github.com/wlandau/crew/issues/217>.

## crew.cluster 0.3.7

CRAN release: 2025-04-14

- Add `n_tasks` to
  [`crew_options_slurm()`](../reference/crew_options_slurm.md)

## crew.cluster 0.3.6

- Fix links.

## crew.cluster 0.3.5

- Add a new `serialization` argument to all controllers.

## crew.cluster 0.3.4

CRAN release: 2025-02-05

- Deprecate retryable options because `crew` 1.0.0 moved away from them.

## crew.cluster 0.3.3

CRAN release: 2024-11-18

- Add `slurm_memory_gigabytes_required` to set `--mem` in SLURM
  ([\#44](https://github.com/wlandau/crew.cluster/issues/44),
  [@multimeric](https://github.com/multimeric)).
- Add `r_arguments` to accept R command line arguments for workers.
- Support `options_metrics`.
- Organize arguments into options lists.
- Use `crashes_error` from `crew`.
- Implement retryable options for memory, cores, GPUs, wall time, and
  SLURM partition
  ([\#48](https://github.com/wlandau/crew.cluster/issues/48),
  [@stemangiola](https://github.com/stemangiola)).
- Set default `seconds_idle` to 300.

## crew.cluster 0.3.2

CRAN release: 2024-07-10

- Add `retry_tasks` argument.

## crew.cluster 0.3.1

CRAN release: 2024-04-24

- Add a SLURM monitor
  ([\#32](https://github.com/wlandau/crew.cluster/issues/32),
  [@brendanf](https://github.com/brendanf)).

## crew.cluster 0.3.0

CRAN release: 2024-02-08

- Create an abstract monitor class for cluster-specific monitor classes
  to inherit from
  ([\#32](https://github.com/wlandau/crew.cluster/issues/32)).
- Require `crew` \>= 0.8.0.
- Raise `seconds_timeout` to 60 across controllers.

## crew.cluster 0.2.0

CRAN release: 2024-01-08

- Re-enable unit tests.
- Require `crew` \>= 0.7.0.
- Add a “monitor” class for SGE clusters to conveniently list and
  terminate jobs.
- Deprecate `command_delete` in favor of `command_terminate`.

## crew.cluster 0.1.4

CRAN release: 2023-12-10

- Refactor methods for development `crew`.
- Encapsulate non-function `R6` members inside the `private` list.
- Encapsulate functions for launch/termination commands inside the `R6`
  `private` list.

## crew.cluster 0.1.3

CRAN release: 2023-10-17

- Deprecate `seconds_exit`
  (<https://github.com/wlandau/crew/issues/125>,
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Deprecate `seconds_interval`
  (<https://github.com/wlandau/crew/issues/131>).
- Add a new `slurm_partition` argument the SLURM controller and launcher
  ([\#24](https://github.com/wlandau/crew.cluster/issues/24),
  [@kkmann](https://github.com/kkmann)).
- Turn on automatic TLS encryption by default.

## crew.cluster 0.1.2

CRAN release: 2023-09-20

- Require `crew` \>= 0.5.0.
- Add a new `slurm_time_minutes` for SLURM wall time
  ([\#1](https://github.com/wlandau/crew.cluster/issues/1),
  [@cfljam](https://github.com/cfljam)).
- Enable TLS.
- Use MB format for memory in SLURM launcher
  ([\#22](https://github.com/wlandau/crew.cluster/issues/22),
  [@kendonB](https://github.com/kendonB)).
- Use `launch_max` and `crew_tls()` from `crew` 0.5.0.

## crew.cluster 0.1.1

CRAN release: 2023-06-15

- Align with breaking changes in `crew` 0.3.0.
- Increase `seconds_launch` to 86400 (1 day).

## crew.cluster 0.1.0

CRAN release: 2023-05-19

- Submit workers asynchronously
  ([\#2](https://github.com/wlandau/crew.cluster/issues/2)).
- Use [`system2()`](https://rdrr.io/r/base/system2.html) instead of
  `processx` to submit workers
  ([\#2](https://github.com/wlandau/crew.cluster/issues/2)).
- Add a `verbose` argument to the SGE launcher to optionally print
  [`system2()`](https://rdrr.io/r/base/system2.html) stdout and stderr.
- Implement SLURM and PBS/TORQUE launchers
  ([\#1](https://github.com/wlandau/crew.cluster/issues/1),
  [\#5](https://github.com/wlandau/crew.cluster/issues/5),
  [\#6](https://github.com/wlandau/crew.cluster/issues/6)).
- Create an abstract cluster launcher class to combine common elements
  of cluster launchers.
- Add a `script_directory` argument to customize the location of job
  scripts ([\#3](https://github.com/wlandau/crew.cluster/issues/3),
  [@mglev1n](https://github.com/mglev1n)).
- Implement an LSF launcher
  ([\#4](https://github.com/wlandau/crew.cluster/issues/4),
  [@mglev1n](https://github.com/mglev1n)).

## crew.cluster 0.0.2

CRAN release: 2023-04-25

- Fix author spelling in the DESCRIPTION file.

## crew.cluster 0.0.1

- First version.
