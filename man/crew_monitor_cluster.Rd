% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crew_monitor_cluster.R
\name{crew_monitor_cluster}
\alias{crew_monitor_cluster}
\title{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} Create an abstract
cluster monitor object.}
\usage{
crew_monitor_cluster(
  verbose = TRUE,
  command_list = as.character(Sys.which("qstat")),
  command_terminate = as.character(Sys.which("qdel"))
)
}
\arguments{
\item{verbose}{Deprecated. Use \code{options_cluster} instead.}

\item{command_list}{Character of length 1,
file path to the executable to list jobs.}

\item{command_terminate}{Deprecated. Use \code{options_cluster} instead.}
}
\description{
Create an abstract cluster monitor \code{R6} object.
}
\seealso{
Other cluster: 
\code{\link{crew_class_launcher_cluster}},
\code{\link{crew_class_monitor_cluster}},
\code{\link{crew_launcher_cluster}()},
\code{\link{crew_options_cluster}()}
}
\concept{cluster}
\keyword{internal}
