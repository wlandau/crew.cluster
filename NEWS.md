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
