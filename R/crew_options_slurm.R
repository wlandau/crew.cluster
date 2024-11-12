#' @title `r lifecycle::badge('experimental')` SLURM options.
#' @export
#' @family slurm
#' @description Set options for SLURM job management.
#' @section Retryable options:
#'   Arguments `memory_gigabytes_required`, `memory_gigabytes_per_cpu`,
#'   `cpus_per_task`, `time_minutes`, and `partition` are retryable options.
#'   Each of these arguments be a vector where each successive element is
#'   used during a retry if the worker previously exited without
#'   completing all its assigned tasks.
#'   The last element of the vector is used if there are more retries than
#'   the length of the vector.
#'   Control the number of allowable retries with `crashes_error`
#'   argument of the controller.
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
#' @param memory_gigabytes_required Positive numeric vector,
#'   usually with a single element.
#'   Supply a vector to make `memory_gigabytes_required` a retryable option
#'   (see the "Retryable options" section for details).
#'
#'   Each element is of `memory_gigabytes_required`
#'   the total number of gigabytes of memory required per node.
#'  `memory_gigabytes_required = 2.40123`
#'   translates to a line of `#SBATCH --mem=2041M`
#'   in the SLURM job script.
#'   `memory_gigabytes_required = NULL` omits this line.
#' @param memory_gigabytes_per_cpu Positive numeric vector,
#'   usually with a single element.
#'   Supply a vector to make `memory_gigabytes_per_cpu` a retryable option
#'   (see the "Retryable options" section for details).
#'
#'   `memory_gigabytes_per_cpu` is the gigabytes of
#'   memory required per CPU.
#'   `memory_gigabytes_per_cpu = 2.40123`
#'   translates to a line of `#SBATCH --mem-per-cpu=2041M`
#'   in the SLURM job script.
#'   `memory_gigabytes_per_cpu = NULL` omits this line.
#' @param cpus_per_task Optional positive integer vector,
#'   usually with a single element.
#'   Supply a vector to make `cpus_per_task` a retryable option
#'   (see the "Retryable options" section for details).
#'
#'   `cpus_per_task` is the number of CPUs for the worker.
#'   `cpus_per_task = 4` translates
#'   to a line of `#SBATCH --cpus-per-task=4` in the SLURM job script.
#'   `cpus_per_task = NULL` omits this line.
#' @param time_minutes Numeric of length 1, usually with a single element.
#'   Supply a vector to make `time_minutes` a retryable option
#'   (see the "Retryable options" section for details).
#'
#'   `time_minutes` is the number of minutes to
#'   designate as the wall time of `crew` each worker instance on the
#'   SLURM cluster. `time_minutes = 60` translates to a line of
#'   `#SBATCH --time=60` in the SLURM job script. `time_minutes = NULL`
#'   omits this line.
#' @param partition Character of vector, usually with a single element.
#'   Supply a vector to make `partition` a retryable option
#'   (see the "Retryable options" section for details).
#'
#'   `partition` is the name of the SLURM partition to
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
