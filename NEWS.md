# crew.cluster 0.0.2.9000 (development)

* Submit workers asynchronously (#2).
* Use `system2()` instead of `processx` to submit workers (#2).
* Add a `verbose` argument to the SGE launcher to optionally print `system2()` stdout and stderr.
* Implement a SLURM launcher.
* Create an abstract cluster launcher class to combine common elements of cluster launchers.
* Add a `script_directory` argument to customize the location of job scripts (#3, @mglev1n).

# crew.cluster 0.0.2

* Fix author spelling in the DESCRIPTION file.

# crew.cluster 0.0.1

* First version.
