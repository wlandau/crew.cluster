#' @title PBS options.
#' @export
#' @family pbs
#' @description Set options for PBS job management.
#' @return A classed list of options.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew_options_cluster
#' @param cwd Logical of length 1, whether to set the working directory
#'   of the worker to the working directory it was launched from.
#'   `cwd = TRUE` is translates to a line of `cd "$O_WORKDIR"`
#'   in the job script. This line is inserted after the content of
#'   `script_lines` to make sure the `#PBS` directives are above
#'   system commands. `cwd = FALSE` omits this line.
#' @param log_output Character of length 1, file or directory path to PBS
#'   worker log files for standard output.
#'   `log_output = "VALUE"` translates to a line of
#'   `#PBS -o VALUE` in the PBS job script. The default is `/dev/null` to omit
#'   the logs. If you do supply a non-`/dev/null` value,
#'   it is recommended to supply a
#'   directory path with a trailing slash so that each worker gets its own set
#'   of log files.
#' @param log_error Character of length 1, file or directory path to PBS
#'   worker log files for standard error.
#'   `log_error = "VALUE"` translates to a line of
#'   `#PBS -e VALUE` in the PBS job script.
#'   The default of `NULL` omits this line.
#'   If you do supply a non-`/dev/null` value, it is recommended to supply a
#'   directory path with a trailing slash so that each worker gets its own set
#'   of log files.
#' @param log_join Logical, whether to join the stdout and stderr log
#'   files together into one file. `log_join = TRUE` translates to a line
#'   of `#PBS -j oe` in the PBS job script, while `log_join = FALSE` is
#'   equivalent to `#PBS -j n`. If `log_join = TRUE`, then `log_error`
#'   should be `NULL`.
#' @param memory_gigabytes_required Optional positive numeric of length 1
#'   with the gigabytes of memory required to run the worker.
#'   `memory_gigabytes_required = 2.4`
#'   translates to a line of `#PBS -l mem=2.4gb` in the PBS job script.
#'   `memory_gigabytes_required = NULL` omits this line.
#' @param cores Optional positive integer of length 1,
#'   number of cores per worker ("slots" in PBS lingo).
#'   `cores = 4` translates
#'   to a line of `#PBS -l ppn=4` in the PBS job script.
#'   `cores = NULL` omits this line.
#' @param walltime_hours Numeric of length 1 with the hours of wall time
#'   to request for the job. `walltime_hours = 23` translates to
#'   a line of `#PBS -l walltime=23:00:00` in the job script.
#'   `walltime_hours = NULL` omits this line.
#' @examples
#'   crew_options_pbs()
crew_options_pbs <- function(
  verbose = FALSE,
  command_submit = as.character(Sys.which("qsub")),
  command_terminate = as.character(Sys.which("qdel")),
  script_directory = tempdir(),
  script_lines = character(0L),
  cwd = TRUE,
  log_output = "/dev/null",
  log_error = NULL,
  log_join = TRUE,
  memory_gigabytes_required = NULL,
  cores = NULL,
  walltime_hours = 12
) {
  out <- structure(
    list(
      verbose = verbose,
      command_submit = command_submit,
      command_terminate = command_terminate,
      script_directory = script_directory,
      script_lines = script_lines,
      cwd = cwd,
      log_output = log_output,
      log_error = log_error,
      log_join = log_join,
      memory_gigabytes_required = memory_gigabytes_required,
      cores = cores,
      walltime_hours = walltime_hours
    ),
    class = c("crew_options_pbs", "crew_options_cluster", "crew_options")
  )
  crew_options_validate(out)
  out
}

#' @export
crew_options_validate.crew_options_pbs <- function(options) {
  NextMethod()
  crew::crew_assert(
    options$log_output,
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    nzchar(.),
    message = "log_output must be a nonempty length-1 character string."
  )
  if (!is.null(options$log_error)) {
    crew::crew_assert(
      options$log_error,
      is.character(.),
      length(.) == 1L,
      !anyNA(.),
      nzchar(.),
      message = paste(
        "log_error must be a nonempty",
        "length-1 character string."
      )
    )
  }
  fields <- c(
    "cwd",
    "log_join"
  )
  for (field in fields) {
    crew::crew_assert(
      options[[field]],
      isTRUE(.) || isFALSE(.),
      message = paste(field, "must be a length-1 logical.")
    )
  }
  fields <- c(
    "memory_gigabytes_required",
    "cores",
    "walltime_hours"
  )
  for (field in fields) {
    if (!is.null(options[[field]])) {
      crew::crew_assert(
        options[[field]],
        is.numeric(.),
        length(.) == 1L,
        !anyNA(.),
        . > 0L,
        message = paste("invalid", field, "field")
      )
    }
  }
}
