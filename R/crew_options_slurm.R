#' @title `r lifecycle::badge('experimental')` SLURM options.
#' @export
#' @family slurm
#' @description Set options for SLURM job management.
#' @section Retryable options:
#'   Retryable options are deprecated in `crew.cluster` as of
#'   2025-01-27 (version `0.3.4`).
#' @return A classed list of options.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew_options_cluster
#' @param log_output Character of length 1, file pattern to control
#'   the locations of the SLURM worker log files. By default, both standard
#'   output and standard error go to the same file.
#'   `log_output = "crew_log_%A.txt"` translates to a line of
#'   `#SBATCH --output=crew_log_%A.txt` in the SLURM job script,
#'   where `%A` is replaced by the job ID of the worker.
#'   The default is `/dev/null` to omit these logs.
#'   Set `log_output = NULL` to omit this line from the job script.
#' @param log_error Character of length 1, file pattern for standard
#'   error. `log_error = "crew_log_%A.txt"` translates to a line of
#'   `#SBATCH --error=crew_log_%A.txt` in the SLURM job script,
#'   where `%A` is replaced by the job ID of the worker.
#'   The default is `/dev/null` to omit these logs.
#'   Set `log_error = NULL` to omit this line from the job script.
#' @param memory_gigabytes_required Positive numeric scalar,
#'   total number of gigabytes of memory required per node.
#'  `memory_gigabytes_required = 2.40123`
#'   translates to a line of `#SBATCH --mem=2041M`
#'   in the SLURM job script.
#'   `memory_gigabytes_required = NULL` omits this line.
#' @param memory_gigabytes_per_cpu Positive numeric scalar,
#'   gigabytes of
#'   memory required per CPU.
#'   `memory_gigabytes_per_cpu = 2.40123`
#'   translates to a line of `#SBATCH --mem-per-cpu=2041M`
#'   in the SLURM job script.
#'   `memory_gigabytes_per_cpu = NULL` omits this line.
#' @param cpus_per_task Optional positive integer scalar,
#'   number of CPUs for the worker.
#'   `cpus_per_task = 4` translates
#'   to a line of `#SBATCH --cpus-per-task=4` in the SLURM job script.
#'   `cpus_per_task = NULL` omits this line.
#' @param time_minutes Numeric scalar,
#'   number of minutes to
#'   designate as the wall time of `crew` each worker instance on the
#'   SLURM cluster. `time_minutes = 60` translates to a line of
#'   `#SBATCH --time=60` in the SLURM job script. `time_minutes = NULL`
#'   omits this line.
#' @param partition Character string,
#'   name of the SLURM partition to
#'   create workers on. `partition = "partition1,partition2"`
#'   translates to a line of `#SBATCH --partition=partition1,partition2`
#'   in the SLURM job script. `partition = NULL`
#'   omits this line.
#' @examples
#'   crew_options_slurm()
crew_options_slurm <- function(
  verbose = FALSE,
  command_submit = as.character(Sys.which("sbatch")),
  command_terminate = as.character(Sys.which("scancel")),
  script_directory = tempdir(),
  script_lines = character(0L),
  log_output = "/dev/null",
  log_error = "/dev/null",
  memory_gigabytes_required = NULL,
  memory_gigabytes_per_cpu = NULL,
  cpus_per_task = NULL,
  time_minutes = NULL,
  partition = NULL
) {
  crew::crew_deprecate(
    name = "Retryable options in crew.cluster",
    date = "2025-01-27",
    version = "0.3.4",
    alternative = paste(
      "none. Please supply scalars for",
      "memory_gigabytes_required, memory_gigabytes_per_cpu, cpus_per_task,",
      "time_minutes, and partition."
    ),
    value = if_any(
      length(memory_gigabytes_required) > 1L ||
        length(memory_gigabytes_per_cpu) > 1L ||
        length(cpus_per_task) > 1L ||
        length(time_minutes) > 1L ||
        length(partition) > 1L,
      TRUE,
      NULL
    ),
    skip_cran = TRUE,
    condition = "message"
  )
  if (!is.null(memory_gigabytes_required)) {
    memory_gigabytes_required <- memory_gigabytes_required[1L]
  }
  if (!is.null(memory_gigabytes_per_cpu)) {
    memory_gigabytes_per_cpu <- memory_gigabytes_per_cpu[1L]
  }
  if (!is.null(cpus_per_task)) {
    cpus_per_task <- cpus_per_task[1L]
  }
  if (!is.null(time_minutes)) {
    time_minutes <- time_minutes[1L]
  }
  if (!is.null(partition)) {
    partition <- partition[1L]
  }
  out <- structure(
    list(
      verbose = verbose,
      command_submit = command_submit,
      command_terminate = command_terminate,
      script_directory = script_directory,
      script_lines = script_lines,
      log_output = log_output,
      log_error = log_error,
      memory_gigabytes_per_cpu = memory_gigabytes_per_cpu,
      memory_gigabytes_required = memory_gigabytes_required,
      cpus_per_task = cpus_per_task,
      time_minutes = time_minutes,
      partition = partition
    ),
    class = c("crew_options_slurm", "crew_options_cluster", "crew_options")
  )
  crew_options_validate(out)
  out
}

#' @export
crew_options_validate.crew_options_slurm <- function(options) {
  NextMethod()
  fields <- c("log_output", "log_error")
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
  if (!is.null(options[["partition"]])) {
    crew::crew_assert(
      options[["partition"]],
      is.character(.),
      length(.) >= 1L,
      !anyNA(.),
      nzchar(.),
      message = c(
        "partition must be either NULL or a nonempty character vector."
      )
    )
  }
  fields <- c(
    "memory_gigabytes_required",
    "memory_gigabytes_per_cpu",
    "cpus_per_task",
    "time_minutes"
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
