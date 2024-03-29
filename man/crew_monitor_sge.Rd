% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_monitor_sge.R
\name{crew_monitor_sge}
\alias{crew_monitor_sge}
\title{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} Create a SGE monitor object.}
\usage{
crew_monitor_sge(
  verbose = TRUE,
  command_list = as.character(Sys.which("qstat")),
  command_terminate = as.character(Sys.which("qdel"))
)
}
\arguments{
\item{verbose}{Logical, whether to see console output and error messages
when submitting worker.}

\item{command_list}{Character of length 1,
file path to the executable to list jobs.}

\item{command_terminate}{Character of length 1,
file path to the executable to terminate a worker job.
Set to \code{""} to skip manually terminating the worker.
Unless there is an issue with the platform,
the job should still exit thanks to the NNG-powered network programming
capabilities of \code{mirai}. Still, if you set \code{command_terminate = ""},
you are assuming extra responsibility for manually monitoring
your jobs on the cluster and manually terminating jobs as appropriate.}
}
\description{
Create an \code{R6} object to monitor SGE cluster jobs.
}
\seealso{
Other sge: 
\code{\link{crew_class_launcher_sge}},
\code{\link{crew_class_monitor_sge}},
\code{\link{crew_controller_sge}()},
\code{\link{crew_launcher_sge}()}
}
\concept{sge}
