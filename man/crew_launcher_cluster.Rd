% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_launcher_cluster.R
\name{crew_launcher_cluster}
\alias{crew_launcher_cluster}
\title{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#maturing}{\figure{lifecycle-maturing.svg}{options: alt='[Maturing]'}}}{\strong{[Maturing]}} Create an abstract cluster
launcher object.}
\usage{
crew_launcher_cluster(
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
  options_cluster = crew.cluster::crew_options_cluster(),
  verbose = NULL,
  command_submit = NULL,
  command_terminate = NULL,
  command_delete = NULL,
  script_directory = NULL,
  script_lines = NULL
)
}
\arguments{
\item{name}{Character string, name of the launcher. If the name is
\code{NULL}, then a name is automatically generated when the launcher starts.}

\item{workers}{Maximum number of workers to run concurrently
when auto-scaling, excluding task retries and manual calls to \code{launch()}.
Special workers allocated for task retries
do not count towards this limit, so the number of workers running at a
given time may exceed this maximum.
A smaller number of workers may run if the number of executing tasks
is smaller than the supplied value of the \code{workers} argument.}

\item{seconds_interval}{Number of seconds between
polling intervals waiting for certain internal
synchronous operations to complete. In certain cases, exponential
backoff is used with this argument passed to \code{seconds_max}
in a \code{\link[crew:crew_throttle]{crew_throttle()}} object.}

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

\item{reset_globals}{Deprecated on 2025-05-30 (\code{crew} version 1.1.2.9004).
Please use the \code{reset_globals} option of \code{\link[crew:crew_controller]{crew_controller()}}
instead.}

\item{reset_packages}{Deprecated on 2025-05-30 (\code{crew} version 1.1.2.9004).
Please use the \code{reset_packages} option of \code{\link[crew:crew_controller]{crew_controller()}}
instead.}

\item{reset_options}{Deprecated on 2025-05-30 (\code{crew} version 1.1.2.9004).
Please use the \code{reset_options} option of \code{\link[crew:crew_controller]{crew_controller()}}
instead.}

\item{garbage_collection}{Deprecated on 2025-05-30
(\code{crew} version 1.1.2.9004).
Please use the \code{garbage_collection} option of
\code{\link[crew:crew_controller]{crew_controller()}} instead.}

\item{crashes_error}{Deprecated on 2025-01-13 (\code{crew} version 0.10.2.9002).}

\item{tls}{A TLS configuration object from \code{\link[crew:crew_tls]{crew_tls()}}.}

\item{r_arguments}{Optional character vector of command line arguments
to pass to \code{Rscript} (non-Windows) or \code{Rscript.exe} (Windows)
when starting a worker. Example:
\code{r_arguments = c("--vanilla", "--max-connections=32")}.}

\item{options_metrics}{Either \code{NULL} to opt out of resource metric logging
for workers, or an object from \code{\link[crew:crew_options_metrics]{crew_options_metrics()}} to enable
and configure resource metric logging for workers.
For resource logging to run,
the \code{autometric} R package version 0.1.0 or higher
must be installed.}

\item{options_cluster}{List of options from a \code{crew.cluster}
options function such as \code{\link[=crew_options_slurm]{crew_options_slurm()}}.
Make sure the cluster types of the launcher and options function match.}

\item{verbose}{Deprecated. Use \code{options_cluster} instead.}

\item{command_submit}{Deprecated. Use \code{options_cluster} instead.}

\item{command_terminate}{Deprecated. Use \code{options_cluster} instead.}

\item{command_delete}{Deprecated on 2024-01-08 (version 0.1.4.9001).
Use \code{command_terminate} instead.}

\item{script_directory}{Deprecated. Use \code{options_cluster} instead.}

\item{script_lines}{Deprecated. Use \code{options_cluster} instead.}
}
\description{
Create an \code{R6} abstract cluster launcher object.
}
\details{
This abstract class is used to develop specific launcher classes
for specific computing platforms.
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
Other cluster: 
\code{\link{crew_class_launcher_cluster}},
\code{\link{crew_class_monitor_cluster}},
\code{\link{crew_monitor_cluster}()},
\code{\link{crew_options_cluster}()}
}
\concept{cluster}
\keyword{internal}
