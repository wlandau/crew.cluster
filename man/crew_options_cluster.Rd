% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_options_cluster.R
\name{crew_options_cluster}
\alias{crew_options_cluster}
\title{Common abstract cluster options.}
\usage{
crew_options_cluster(
  verbose = FALSE,
  command_submit = as.character(Sys.which("qsub")),
  command_terminate = as.character(Sys.which("qdel")),
  script_directory = tempdir(),
  script_lines = character(0L)
)
}
\arguments{
\item{verbose}{Logical, whether to see console output and error messages
when submitting worker.}

\item{command_submit}{Character of length 1,
file path to the executable to submit a worker job.}

\item{command_terminate}{Character of length 1,
file path to the executable to terminate a worker job.
Set to \code{""} to skip manually terminating the worker.
Unless there is an issue with the platform,
the job should still exit thanks to the NNG-powered network programming
capabilities of \code{mirai}. Still, if you set \code{command_terminate = ""},
you are assuming extra responsibility for manually monitoring
your jobs on the cluster and manually terminating jobs as appropriate.}

\item{script_directory}{Character of length 1, directory path to the
job scripts. Just before each job submission, a job script
is created in this folder. Script base names are unique to each
launcher and worker, and the launcher deletes the script when the
worker is manually terminated. \code{tempdir()} is the default, but it
might not work for some systems.
\code{tools::R_user_dir("crew.cluster", which = "cache")}
is another reasonable choice.}

\item{script_lines}{Optional character vector of additional lines to be
added to the job script just after the more common flags.
An example would be \code{script_lines = "module load R"} if your cluster
supports R through an environment module.}
}
\value{
A classed list of options.
}
\description{
Common abstract cluster options.
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
  crew_options_cluster()
}
\seealso{
Other cluster: 
\code{\link{crew_class_launcher_cluster}},
\code{\link{crew_class_monitor_cluster}},
\code{\link{crew_launcher_cluster}()},
\code{\link{crew_monitor_cluster}()}
}
\concept{cluster}
\keyword{internal}
