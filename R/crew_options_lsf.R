#' @title `r lifecycle::badge('experimental')` LSF options.
#' @export
#' @family lsf
#' @description Set options for LSF job management.
#' @section Retryable options:
#'   Retryable options are deprecated in `crew.cluster` as of
#'   2025-01-27 (version `0.3.4`).
#' @return A classed list of options.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew_options_cluster
#' @param cwd Character of length 1, directory to
#'   launch the worker from (as opposed to
#'   the system default). `cwd = "/home"` translates to a line of
#'   `#BSUB -cwd /home` in the LSF job script. `cwd = getwd()` is the
#'   default, which launches workers from the current working directory.
#'   Set `cwd = NULL` to omit this line from the job script.
#' @param log_output Character of length 1, file pattern to control
#'   the locations of the LSF worker log files. By default, both standard
#'   output and standard error go to the same file.
#'   `log_output = "crew_log_%J.log"` translates to a line of
#'   `#BSUB -o crew_log_%J.log` in the LSF job script,
#'   where `%J` is replaced by the job ID of the worker.
#'   The default is `/dev/null` to omit these logs.
#'   Set `log_output = NULL` to omit this line from the job script.
#' @param log_error Character of length 1, file pattern for standard
#'   error. `log_error = "crew_error_%J.err"` translates to a line of
#'   `#BSUB -e crew_error_%J.err` in the LSF job script,
#'   where `%J` is replaced by the job ID of the worker.
#'   The default is `/dev/null` to omit these logs.
#'   Set `log_error = NULL` to omit this line from the job script.
#' @param memory_gigabytes_limit Positive numeric scalar, memory
#'   limit in gigabytes of the worker.
#'   `memory_gigabytes_limit = 4`
#'   translates to a line of `#BSUB -M 4G`
#'   in the LSF job script.
#'   `memory_gigabytes_limit = NULL` omits this line.
#' @param memory_gigabytes_required Positive numeric scalar,
#'   memory requirement in gigabytes.
#'   `memory_gigabytes_required = 4`
#'   translates to a line of `#BSUB -R 'rusage[mem=4G]'`
#'   in the LSF job script.
#'   `memory_gigabytes_required = NULL` omits this line.
#' @param cores Optional positive integer scalar,
#'   number of CPU cores for the worker.
#'   `cores = 4` translates
#'   to a line of `#BSUB -n 4` in the LSF job script.
#'   `cores = NULL` omits this line.
#' @examples
#'   crew_options_lsf()
crew_options_lsf <- function(
  verbose = FALSE,
  command_submit = as.character(Sys.which("bsub")),
  command_terminate = NULL,
  script_directory = tempdir(),
  script_lines = character(0L),
  cwd = getwd(),
  log_output = "/dev/null",
  log_error = "/dev/null",
  memory_gigabytes_limit = NULL,
  memory_gigabytes_required = NULL,
  cores = NULL
) {
  crew::crew_deprecate(
    name = "Retryable options in crew.cluster",
    date = "2025-01-27",
    version = "0.3.4",
    alternative = paste(
      "none. Please supply scalars for memory_gigabytes_limit,",
      "memory_gigabytes_required, and cores"
    ),
    value = if_any(
      length(memory_gigabytes_limit) > 1L ||
        length(memory_gigabytes_required) > 1L ||
        length(cores) > 1L,
      TRUE,
      NULL
    ),
    skip_cran = TRUE,
    condition = "message"
  )
  crew::crew_deprecate(
    name = "command_terminate",
    date = "2025-08-26",
    version = "0.3.8.9001",
    alternative = paste(
      "none (no longer needed,
      c.f. https://github.com/wlandau/crew/issues/236)."
    ),
    condition = "message",
    value = command_terminate
  )
  if (!is.null(memory_gigabytes_limit)) {
    memory_gigabytes_limit <- memory_gigabytes_limit[1L]
  }
  if (!is.null(memory_gigabytes_required)) {
    memory_gigabytes_required <- memory_gigabytes_required[1L]
  }
  if (!is.null(cores)) {
    cores <- cores[1L]
  }
  out <- structure(
    list(
      verbose = verbose,
      command_submit = command_submit,
      script_directory = script_directory,
      script_lines = script_lines,
      cwd = cwd,
      log_output = log_output,
      log_error = log_error,
      memory_gigabytes_limit = memory_gigabytes_limit,
      memory_gigabytes_required = memory_gigabytes_required,
      cores = cores
    ),
    class = c("crew_options_lsf", "crew_options_cluster", "crew_options")
  )
  crew_options_validate(out)
  out
}

#' @export
crew_options_validate.crew_options_lsf <- function(options) {
  NextMethod()
  fields <- c("log_output", "log_error", "cwd")
  for (field in fields) {
    if (!is.null(options[[field]])) {
      crew::crew_assert(
        options[[field]],
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        message = paste(
          field,
          "must be either NULL or a nonempty length-1 character string."
        )
      )
    }
  }
  fields <- c(
    "memory_gigabytes_limit",
    "memory_gigabytes_required",
    "cores"
  )
  for (field in fields) {
    if (!is.null(options[[field]])) {
      crew::crew_assert(
        options[[field]],
        is.numeric(.),
        length(.) >= 1L,
        !anyNA(.),
        . > 0L,
        message = paste("invalid", field, "field")
      )
    }
  }
}
