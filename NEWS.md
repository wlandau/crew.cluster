# crew.cluster 0.3.1

* Add a monitor class for SLURM clusters.

# crew.cluster 0.3.0

* Create an abstract monitor class for cluster-specific monitor classes to inherit from (#32).
* Require `crew` >= 0.8.0.
* Raise `seconds_timeout` to 60 across controllers.

# crew.cluster 0.2.0

* Re-enable unit tests.
* Require `crew` >= 0.7.0.
* Add a "monitor" class for SGE clusters to conveniently list and terminate jobs.
* Deprecate `command_delete` in favor of `command_terminate`.

# crew.cluster 0.1.4

* Refactor methods for development `crew`.
* Encapsulate non-function `R6` members inside the `private` list.
* Encapsulate functions for launch/termination commands inside the `R6` `private` list.

# crew.cluster 0.1.3

* Deprecate `seconds_exit` (https://github.com/wlandau/crew/issues/125, @shikokuchuo).
* Deprecate `seconds_interval` (https://github.com/wlandau/crew/issues/131).
* Add a new `slurm_partition` argument the SLURM controller and launcher (#24, @kkmann).
* Turn on automatic TLS encryption by default.

# crew.cluster 0.1.2

* Require `crew` >= 0.5.0.
* Add a new `slurm_time_minutes` for SLURM wall time (#1, @cfljam).
* Enable TLS.
* Use MB format for memory in SLURM launcher (#22, @kendonB).
* Use `launch_max` and `crew_tls()` from `crew` 0.5.0.

# crew.cluster 0.1.1

* Align with breaking changes in `crew` 0.3.0.
* Increase `seconds_launch` to 86400 (1 day).

# crew.cluster 0.1.0

* Submit workers asynchronously (#2).
* Use `system2()` instead of `processx` to submit workers (#2).
* Add a `verbose` argument to the SGE launcher to optionally print `system2()` stdout and stderr.
* Implement SLURM and PBS/TORQUE launchers (#1, #5, #6).
* Create an abstract cluster launcher class to combine common elements of cluster launchers.
* Add a `script_directory` argument to customize the location of job scripts (#3, @mglev1n).
* Implement an LSF launcher (#4, @mglev1n).

# crew.cluster 0.0.2

* Fix author spelling in the DESCRIPTION file.

# crew.cluster 0.0.1

* First version.
