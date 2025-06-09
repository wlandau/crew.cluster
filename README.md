
# crew.cluster: crew launcher plugins for traditional high-performance computing clusters <img src='man/figures/logo-readme.png' align="right" height="139"/>

[![CRAN](https://www.r-pkg.org/badges/version/crew.cluster)](https://CRAN.R-project.org/package=crew.cluster)
[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#Active)
[![check](https://github.com/wlandau/crew.cluster/actions/workflows/check.yaml/badge.svg)](https://github.com/wlandau/crew.cluster/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/wlandau/crew.cluster/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://app.codecov.io/gh/wlandau/crew.cluster)
[![lint](https://github.com/wlandau/crew.cluster/actions/workflows/lint.yaml/badge.svg)](https://github.com/wlandau/crew.cluster/actions?query=workflow%3Alint)
[![pkgdown](https://github.com/wlandau/crew.cluster/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/wlandau/crew.cluster/actions?query=workflow%3Apkgdown)

In computationally demanding analysis projects, statisticians and data
scientists asynchronously deploy long-running tasks to distributed
systems, ranging from traditional clusters to cloud services. The
`crew.cluster` package extends the
[`mirai`](https://github.com/r-lib/mirai)-powered ‘crew’ package with
worker launcher plugins for traditional high-performance computing
systems. Inspiration also comes from packages
[`mirai`](https://github.com/r-lib/mirai),
[`future`](https://future.futureverse.org/),
[`rrq`](https://mrc-ide.github.io/rrq/),
[`clustermq`](https://mschubert.github.io/clustermq/), and
[`batchtools`](https://batchtools.mlr-org.com).

# Installation

| Type | Source | Command |
|----|----|----|
| Release | CRAN | `install.packages("crew.cluster")` |
| Development | GitHub | `remotes::install_github("wlandau/crew.cluster")` |
| Development | R-universe | `install.packages("crew.cluster", repos = "https://wlandau.r-universe.dev")` |

# Documentation

Please see <https://wlandau.github.io/crew.cluster/> for documentation,
including a full function reference and usage tutorial.

# Usage

First, create a controller object appropriate for your platform. For
example, to launch workers on a Sun Grid Engine (SGE) cluster, use
`crew_controller_sge()`.

``` r
library(crew.cluster)
controller <- crew_controller_sge(
  name = "my_workflow", # for informative job names
  workers = 16,
  tasks_max = 2, # to avoid reaching wall time limits
  seconds_idle = 10, # to release resources when they are not needed,
  # Try 16 GB memory first, then use 32 GB to retry if the worker crashes,
  # then 64 GB for all subsequent retries after failure. Go back to 16 GB
  # if the worker completes all its tasks before exiting.
  sge_memory_gigabytes_required = c(16, 32, 64),
  script_lines = "module load R" # if R is an environment module
)
controller$start()
```

At this point, usage is exactly the same as basic
[`crew`](https://wlandau.github.io/crew/). The `push()` method submits
tasks and auto-scales SGE workers to meet demand.

``` r
controller$push(name = "do work", command = do_work())
```

The `pop()` method retrieves available tasks.

``` r
controller$pop()
#> # A tibble: 1 × 11
#>   name         command result seconds   seed error trace warni…¹ launc…² worker insta…³
#>   <chr>        <chr>   <list>   <dbl>  <int> <chr> <chr> <chr>   <chr>    <int> <chr>  
#> 1 do work   … do_work… <int>        0 1.56e8 NA    NA    NA      79e71c…      1 7686b2…
#> # … with abbreviated variable names ¹​warnings, ²​launcher, ³​instance
```

Remember to terminate the controller when you are done.

``` r
controller$terminate()
```

# Monitoring

To manage resource usage, you may choose to list and manually terminate
cluster jobs using `crew_monitor_sge()` and other supported monitors.
Example for SGE:

``` r
monitor <- crew_monitor_sge()
job_list <- monitor$jobs()
job_list
#> # A tibble: 2 × 9
#>   job_number prio    name    owner state start_time queue_name jclass_name slots
#>   <chr>      <chr>   <chr>   <chr> <chr> <chr>      <chr>      <lgl>       <chr>
#> 1 131853812  0.05000 crew-m… USER… r     2024-01-0… all.norma… NA          1    
#> 2 131853813  0.05000 crew-m… USER… r     2024-01-0… all.norma… NA          1
monitor$terminate(jobs = job_list$job_number)
#> USER has registered the job 131853812 for deletion
#> USER has registered the job 131853813 for deletion
monitor$jobs()
#> data frame with 0 columns and 0 rows
```

`monitor$terminate(all = TRUE)` terminates all your SGE jobs, regardless
of whether `crew.cluster` created them.

# Tips

- `crew.cluster` submits jobs over the local network using system calls
  to the resource manager (e.g SGE or SLURM). Please invoke
  `crew.cluster` on a node of the cluster, either a login node (head
  node) or a compute node.
- Most clusters install software like R in versioned environment
  modules. Most likely, you will need to call `module load R` (or
  `module load R/x.y.z` for a specific version) in order to use on the
  cluster. In `crew.cluster`, you will most likely need to supply
  `"module load R"` or similar to the `script_lines` argument of
  e.g. `crew_controller_sge()`.

# Risks

The risks of `crew.cluster` are the same as [those of
`crew`](https://wlandau.github.io/crew/#risks), plus the risks of
traditional high-performance computing environments. These distributed
systems typically operate inside a firewall and trust the local network.
It is your responsibility to assess the security of these systems and
use `crew.cluster` in a safe manner. In addition, `crew.cluster`
automatically launches jobs on the cluster scheduler, and it may not
always be able to terminate leftover jobs. It is your responsibility to
monitor your jobs and manually terminate jobs that `crew.cluster` may
not be able to.

# Thanks

- [Charlie Gao](https://github.com/shikokuchuo) created
  [`mirai`](https://github.com/r-lib/mirai) and
  [`nanonext`](https://github.com/r-lib/nanonext) and graciously
  accommodated the complicated and demanding feature requests that made
  `crew` and its ecosystem possible.
- Thanks to [Michael Schubert](https://github.com/mschubert) for sharing
  the [template
  files](https://github.com/mschubert/clustermq/tree/master/inst) in
  [`clustermq`](https://github.com/mschubert/clustermq) under the
  permissive [Apache License
  2.0](https://github.com/mschubert/clustermq/blob/master/LICENSE).
  These scripts helped construct launcher plugins to clusters where
  direct access was not possible. See the
  [`LICENSE.note`](https://github.com/wlandau/crew.cluster/blob/main/LICENSE.note)
  file in this package.

# Code of Conduct

Please note that the `crew` project is released with a [Contributor Code
of
Conduct](https://github.com/wlandau/crew/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

# Citation

``` r
citation("crew.cluster")
```
