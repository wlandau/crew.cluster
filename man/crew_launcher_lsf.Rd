% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_launcher_lsf.R
\name{crew_launcher_lsf}
\alias{crew_launcher_lsf}
\title{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} Create a launcher with
LSF workers.}
\usage{
crew_launcher_lsf(
  name = NULL,
  seconds_interval = 0.5,
  seconds_timeout = 60,
  seconds_launch = 86400,
  seconds_idle = Inf,
  seconds_wall = Inf,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  launch_max = 5L,
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
}
\arguments{
\item{name}{Name of the launcher.}

\item{seconds_interval}{Number of seconds between
polling intervals waiting for certain internal
synchronous operations to complete,
such as checking \code{mirai::status()}.}

\item{seconds_timeout}{Number of seconds until timing
out while waiting for certain synchronous operations to complete,
such as checking \code{mirai::status()}.}

\item{seconds_launch}{Seconds of startup time to allow.
A worker is unconditionally assumed to be alive
from the moment of its launch until \code{seconds_launch} seconds later.
After \code{seconds_launch} seconds, the worker is only
considered alive if it is actively connected to its assign websocket.}

\item{seconds_idle}{Maximum number of seconds that a worker can idle
since the completion of the last task. If exceeded, the worker exits.
But the timer does not launch until \code{tasks_timers} tasks
have completed.
See the \code{idletime} argument of \code{mirai::daemon()}. \code{crew} does not
excel with perfectly transient workers because it does not micromanage
the assignment of tasks to workers, so please allow enough idle
time for a new worker to be delegated a new task.}

\item{seconds_wall}{Soft wall time in seconds.
The timer does not launch until \code{tasks_timers} tasks
have completed.
See the \code{walltime} argument of \code{mirai::daemon()}.}

\item{tasks_max}{Maximum number of tasks that a worker will do before
exiting. See the \code{maxtasks} argument of \code{mirai::daemon()}.
\code{crew} does not
excel with perfectly transient workers because it does not micromanage
the assignment of tasks to workers, it is recommended to set
\code{tasks_max} to a value greater than 1.}

\item{tasks_timers}{Number of tasks to do before activating
the timers for \code{seconds_idle} and \code{seconds_wall}.
See the \code{timerstart} argument of \code{mirai::daemon()}.}

\item{reset_globals}{\code{TRUE} to reset global environment
variables between tasks, \code{FALSE} to leave them alone.}

\item{reset_packages}{\code{TRUE} to unload any packages loaded during
a task (runs between each task), \code{FALSE} to leave packages alone.}

\item{reset_options}{\code{TRUE} to reset global options to their original
state between each task, \code{FALSE} otherwise. It is recommended to
only set \code{reset_options = TRUE} if \code{reset_packages} is also \code{TRUE}
because packages sometimes rely on options they set at loading time.}

\item{garbage_collection}{\code{TRUE} to run garbage collection between
tasks, \code{FALSE} to skip.}

\item{launch_max}{Positive integer of length 1, maximum allowed
consecutive launch attempts which do not complete any tasks.
Enforced on a worker-by-worker basis.
The futile launch count resets to back 0
for each worker that completes a task.
It is recommended to set \code{launch_max} above 0
because sometimes workers are unproductive under perfectly ordinary
circumstances. But \code{launch_max} should still be small enough
to detect errors in the underlying platform.}

\item{tls}{A TLS configuration object from \code{\link[crew:crew_tls]{crew_tls()}}.}

\item{r_arguments}{Optional character vector of command line arguments
to pass to \code{Rscript} (non-Windows) or \code{Rscript.exe} (Windows)
when starting a worker. Example:
\code{r_arguments = c("--vanilla", "--max-connections=32")}.}

\item{options_metrics}{Either \code{NULL} to opt out of resource metric logging
for workers, or an object from \code{\link[crew:crew_options_metrics]{crew_options_metrics()}} to enable
and configure resource metric logging for workers.}

\item{options_cluster}{An options list from
\code{\link[=crew_options_lsf]{crew_options_lsf()}} with cluster-specific configuration options.}

\item{verbose}{Deprecated. Use \code{options_cluster} instead.}

\item{command_submit}{Deprecated. Use \code{options_cluster} instead.}

\item{command_terminate}{Deprecated. Use \code{options_cluster} instead.}

\item{command_delete}{Deprecated on 2024-01-08 (version 0.1.4.9001).
Use \code{command_terminate} instead.}

\item{script_directory}{Deprecated. Use \code{options_cluster} instead.}

\item{script_lines}{Deprecated. Use \code{options_cluster} instead.}

\item{lsf_cwd}{Deprecated. Use \code{options_cluster} instead.}

\item{lsf_log_output}{Deprecated. Use \code{options_cluster} instead.}

\item{lsf_log_error}{Deprecated. Use \code{options_cluster} instead.}

\item{lsf_memory_gigabytes_limit}{Deprecated.
Use \code{options_cluster} instead.}

\item{lsf_memory_gigabytes_required}{Deprecated.
Use \code{options_cluster} instead.}

\item{lsf_cores}{Deprecated. Use \code{options_cluster} instead.}
}
\description{
Create an \code{R6} object to launch and maintain
workers as LSF jobs.
}
\details{
WARNING: the \code{crew.cluster} LSF plugin is experimental.
Please proceed with caution and report bugs to
\url{https://github.com/wlandau/crew.cluster}.

To launch a LSF worker, this launcher
creates a temporary job script with a call to \code{crew::crew_worker()}
and submits it as an LSF job with \code{sbatch}. To see most of the lines
of the job script in advance, use the \code{script()} method of the launcher.
It has all the lines except for the job name and the
call to \code{crew::crew_worker()}, both of
which will be inserted at the last minute when it is time
to actually launch a worker.
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

\seealso{
Other lsf: 
\code{\link{crew_class_launcher_lsf}},
\code{\link{crew_controller_lsf}()},
\code{\link{crew_options_lsf}()}
}
\concept{lsf}
