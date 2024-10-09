#' @title SGE options.
#' @export
#' @family sge
#' @description Set options for SGE job management.
#' @return A classed list of options.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew_options_lsf
#' @param cwd Logical of length 1, whether to
#'   launch the worker from the current working directory (as opposed to
#'   the user home directory). `cwd = TRUE` translates to a line of
#'   `#$ -cwd` in the SGE job script. `cwd = FALSE` omits this line.
#' @param envvars Logical of length 1, whether to forward the environment
#'   variables of the current session to the SGE worker. `envvars = TRUE`
#'   translates to a line of `#$ -V` in the SGE job script.
#'   `envvars = FALSE` omits this line.
#' @param log_output Character of length 1, file or directory path to SGE
#'   worker log files for standard output.
#'   `log_output = "VALUE"` translates to a line of
#'   `#$ -o VALUE` in the SGE job script. The default is `/dev/null` to omit
#'   the logs. If you do supply a non-`/dev/null` value,
#'   it is recommended to supply a
#'   directory path with a trailing slash so that each worker gets its own set
#'   of log files.
#' @param log_error Character of length 1, file or directory path to SGE
#'   worker log files for standard error.
#'   `log_error = "VALUE"` translates to a line of
#'   `#$ -e VALUE` in the SGE job script.
#'   The default of `NULL` omits this line.
#'   If you do supply a non-`/dev/null` value, it is recommended to supply a
#'   directory path with a trailing slash so that each worker gets its own set
#'   of log files.
#' @param log_join Logical, whether to join the stdout and stderr log
#'   files together into one file. `log_join = TRUE` translates to a line
#'   of `#$ -j y` in the SGE job script, while `log_join = FALSE` is
#'   equivalent to `#$ -j n`. If `log_join = TRUE`, then `log_error`
#'   should be `NULL`.
#' @param memory_gigabytes_limit Optional numeric of length 1
#'   with the maximum number of gigabytes of memory a worker is allowed to
#'   consume. If the worker consumes more than this level of memory, then
#'   SGE will terminate it. `memory_gigabytes_limit = 5.7"`
#'   translates to a line of `"#$ -l h_rss=5.7G"` in the SGE job script.
#'   `memory_gigabytes_limit = NULL` omits this line.
#' @param memory_gigabytes_required Optional positive numeric of length 1
#'   with the gigabytes of memory required to run the worker.
#'   `memory_gigabytes_required = 2.4`
#'   translates to a line of `#$ -l m_mem_free=2.4G` in the SGE job script.
#'   `memory_gigabytes_required = NULL` omits this line.
#' @param cores Optional positive integer of length 1,
#'   number of cores per worker ("slots" in SGE lingo).
#'   `cores = 4` translates
#'   to a line of `#$ -pe smp 4` in the SGE job script.
#'   `cores = NULL` omits this line.
#' @param gpu Optional integer of length 1 with the number of GPUs to
#'   request for the worker. `gpu = 1` translates to a line of
#'   `"#$ -l gpu=1"` in the SGE job script. `gpu = NULL` omits this line.
#' @examples
#'   crew_options_sge()
crew_options_sge <- function(
  verbose = FALSE,
  command_submit = as.character(Sys.which("qsub")),
  command_terminate = as.character(Sys.which("qdel")),
  command_delete = NULL,
  script_directory = tempdir(),
  script_lines = character(0L),
  cwd = TRUE,
  envvars = FALSE,
  log_output = "/dev/null",
  log_error = NULL,
  log_join = TRUE,
  memory_gigabytes_limit = NULL,
  memory_gigabytes_required = NULL,
  cores = NULL,
  gpu = NULL
) {
  out <- structure(
    list(
      verbose = verbose,
      command_submit = command_submit,
      command_terminate = command_terminate,
      script_directory = script_directory,
      script_lines = script_lines,
      cwd = cwd,
      envvars = envvars,
      log_output = log_output,
      log_error = log_error,
      log_join = log_join,
      memory_gigabytes_limit = memory_gigabytes_limit,
      memory_gigabytes_required = memory_gigabytes_required,
      cores = cores,
      gpu = gpu
    ),
    class = c("crew_options_sge", "crew_options_cluster", "crew_options")
  )
  crew_options_validate(out)
  out
}

#' @export
crew_options_validate.crew_options_sge <- function(options) {
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
    "envvars",
    "log_join"
  )
  for (field in fields) {
    crew::crew_assert(
      options[[field]],
      isTRUE(.) || isFALSE(.),
      message = paste(field, "is not a length-1 logical.")
    )
  }
  fields <- c(
    "memory_gigabytes_limit",
    "memory_gigabytes_required",
    "cores",
    "gpu"
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
