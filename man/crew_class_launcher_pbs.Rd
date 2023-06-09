% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_launcher_pbs.R
\name{crew_class_launcher_pbs}
\alias{crew_class_launcher_pbs}
\title{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#maturing}{\figure{lifecycle-maturing.svg}{options: alt='[Maturing]'}}}{\strong{[Maturing]}} PBS/TORQUE launcher class}
\description{
\code{R6} class to launch and manage PBS/TORQUE workers.
}
\details{
See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.
}
\section{Attribution}{

The template files at
\url{https://github.com/mschubert/clustermq/tree/master/inst}
informed the development of the \code{crew} launcher plugins in
\code{crew.cluster}, and we would like to thank
Michael Schubert for developing \code{clustermq} and releasing it under
the permissive Apache License 2.0.
See the \code{NOTICE} and \code{README.md} files in the \code{crew.cluster}
source code for additional attribution.
}

\examples{

## ------------------------------------------------
## Method `crew_class_launcher_pbs$script`
## ------------------------------------------------

if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
launcher <- crew_launcher_pbs(
  pbs_cores = 2,
  pbs_memory_gigabytes_required = 4
)
launcher$script(name = "my_job_name")
}
}
\seealso{
Other launchers: 
\code{\link{crew_class_launcher_cluster}},
\code{\link{crew_class_launcher_lsf}},
\code{\link{crew_class_launcher_sge}},
\code{\link{crew_class_launcher_slurm}},
\code{\link{crew_launcher_cluster}()},
\code{\link{crew_launcher_lsf}()},
\code{\link{crew_launcher_pbs}()},
\code{\link{crew_launcher_sge}()},
\code{\link{crew_launcher_slurm}()}
}
\concept{launchers}
\section{Super classes}{
\code{\link[crew:crew_class_launcher]{crew::crew_class_launcher}} -> \code{\link[crew.cluster:crew_class_launcher_cluster]{crew.cluster::crew_class_launcher_cluster}} -> \code{crew_class_launcher_pbs}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{pbs_cwd}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_log_output}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_log_error}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_log_join}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_memory_gigabytes_required}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_cores}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_walltime_hours}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-crew_class_launcher_pbs-new}{\code{crew_class_launcher_pbs$new()}}
\item \href{#method-crew_class_launcher_pbs-validate}{\code{crew_class_launcher_pbs$validate()}}
\item \href{#method-crew_class_launcher_pbs-script}{\code{crew_class_launcher_pbs$script()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="backlogged"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-backlogged'><code>crew::crew_class_launcher$backlogged()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="call"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-call'><code>crew::crew_class_launcher$call()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="done"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-done'><code>crew::crew_class_launcher$done()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="launch"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-launch'><code>crew::crew_class_launcher$launch()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="resolved"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-resolved'><code>crew::crew_class_launcher$resolved()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="rotate"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-rotate'><code>crew::crew_class_launcher$rotate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="scale"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-scale'><code>crew::crew_class_launcher$scale()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="settings"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-settings'><code>crew::crew_class_launcher$settings()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="start"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-start'><code>crew::crew_class_launcher$start()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="summary"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-summary'><code>crew::crew_class_launcher$summary()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="tally"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-tally'><code>crew::crew_class_launcher$tally()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="terminate"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-terminate'><code>crew::crew_class_launcher$terminate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="throttle"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-throttle'><code>crew::crew_class_launcher$throttle()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew" data-topic="crew_class_launcher" data-id="unlaunched"><a href='../../crew/html/crew_class_launcher.html#method-crew_class_launcher-unlaunched'><code>crew::crew_class_launcher$unlaunched()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew.cluster" data-topic="crew_class_launcher_cluster" data-id="args_launch"><a href='../../crew.cluster/html/crew_class_launcher_cluster.html#method-crew_class_launcher_cluster-args_launch'><code>crew.cluster::crew_class_launcher_cluster$args_launch()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew.cluster" data-topic="crew_class_launcher_cluster" data-id="args_terminate"><a href='../../crew.cluster/html/crew_class_launcher_cluster.html#method-crew_class_launcher_cluster-args_terminate'><code>crew.cluster::crew_class_launcher_cluster$args_terminate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew.cluster" data-topic="crew_class_launcher_cluster" data-id="launch_worker"><a href='../../crew.cluster/html/crew_class_launcher_cluster.html#method-crew_class_launcher_cluster-launch_worker'><code>crew.cluster::crew_class_launcher_cluster$launch_worker()</code></a></span></li>
<li><span class="pkg-link" data-pkg="crew.cluster" data-topic="crew_class_launcher_cluster" data-id="terminate_worker"><a href='../../crew.cluster/html/crew_class_launcher_cluster.html#method-crew_class_launcher_cluster-terminate_worker'><code>crew.cluster::crew_class_launcher_cluster$terminate_worker()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-crew_class_launcher_pbs-new"></a>}}
\if{latex}{\out{\hypertarget{method-crew_class_launcher_pbs-new}{}}}
\subsection{Method \code{new()}}{
PBS/TORQUE launcher constructor.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{crew_class_launcher_pbs$new(
  name = NULL,
  seconds_interval = NULL,
  seconds_launch = NULL,
  seconds_idle = NULL,
  seconds_wall = NULL,
  seconds_exit = NULL,
  tasks_max = NULL,
  tasks_timers = NULL,
  reset_globals = NULL,
  reset_packages = NULL,
  reset_options = NULL,
  garbage_collection = NULL,
  verbose = NULL,
  command_submit = NULL,
  command_delete = NULL,
  script_directory = NULL,
  script_lines = NULL,
  pbs_cwd = NULL,
  pbs_log_output = NULL,
  pbs_log_error = NULL,
  pbs_log_join = NULL,
  pbs_memory_gigabytes_required = NULL,
  pbs_cores = NULL,
  pbs_walltime_hours = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{seconds_interval}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{seconds_launch}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{seconds_idle}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{seconds_wall}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{seconds_exit}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{tasks_max}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{tasks_timers}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{reset_globals}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{reset_packages}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{reset_options}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{garbage_collection}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{verbose}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{command_submit}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{command_delete}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{script_directory}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{script_lines}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_cwd}}{See \code{\link[=crew_launcher_sge]{crew_launcher_sge()}}.}

\item{\code{pbs_log_output}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_log_error}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_log_join}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_memory_gigabytes_required}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_cores}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}

\item{\code{pbs_walltime_hours}}{See \code{\link[=crew_launcher_pbs]{crew_launcher_pbs()}}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
an PBS/TORQUE launcher object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-crew_class_launcher_pbs-validate"></a>}}
\if{latex}{\out{\hypertarget{method-crew_class_launcher_pbs-validate}{}}}
\subsection{Method \code{validate()}}{
Validate the launcher.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{crew_class_launcher_pbs$validate()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{NULL} (invisibly). Throws an error if a field is invalid.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-crew_class_launcher_pbs-script"></a>}}
\if{latex}{\out{\hypertarget{method-crew_class_launcher_pbs-script}{}}}
\subsection{Method \code{script()}}{
Generate the job script.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{crew_class_launcher_pbs$script(name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{Character of length 1, name of the job. For inspection
purposes, you can supply a mock job name.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Includes everything except the worker-instance-specific
job name and the worker-instance-specific
call to \code{crew::crew_worker()}, both of which get inserted at
the bottom of the script at launch time.
}

\subsection{Returns}{
Character vector of the lines of the job script.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
launcher <- crew_launcher_pbs(
  pbs_cores = 2,
  pbs_memory_gigabytes_required = 4
)
launcher$script(name = "my_job_name")
}
}
\if{html}{\out{</div>}}

}

}
}
