#' @title `r lifecycle::badge('experimental')` Create a launcher with
#'   SLURM workers.
#' @export
#' @family launchers
#' @description Create an `R6` object to launch and maintain
#'   workers as SLURM jobs.
#' @details WARNING: the `crew.cluster` SLURM plugin is experimental
#'   and has not actually been tested on a SLURM cluster. Please proceed
#'   with caution and report bugs to
#'   <https://github.com/wlandau/crew.cluster>.
#'
#'   To launch a SLURM worker, this launcher
#'   creates a temporary job script with a call to `crew::crew_worker()`
#'   and submits it as an SLURM job with `sbatch`. To see most of the lines
#'   of the job script in advance, use the `script()` method of the launcher.
#'   It has all the lines except for the job name and the
#'   call to `crew::crew_worker()`, both of
#'   which will be inserted at the last minute when it is time
#'   to actually launch a worker.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew_launcher_cluster
#' @param slurm_log_output Character of length 1, file pattern to control
#'   the locations of the SLURM worker log files. By default, both standard
#'   output and standard error go to the same file.
#'   `slurm_log_output = "crew_log_%A.txt"` translates to a line of
#'   `#SBATCH --output=crew_log_%A.txt` in the SLURM job script,
#'   where `%A` is replaced by the job ID of the worker.
#'   The default is `/dev/null` to omit these logs.
#'   Set `slurm_log_output = NULL` to omit this line from the job script.
#' @param slurm_log_error Character of length 1, file pattern for standard
#'   error. `slurm_log_error = "crew_log_%A.txt"` translates to a line of
#'   `#SBATCH --error=crew_log_%A.txt` in the SLURM job script,
#'   where `%A` is replaced by the job ID of the worker.
#'   The default is `/dev/null` to omit these logs.
#'   Set `slurm_log_error = NULL` to omit this line from the job script.
#' @param slurm_memory_gigabytes_per_cpu Positive numeric of length 1
#'   with the gigabytes of memory required per CPU.
#'   `slurm_memory_gigabytes_per_cpu = 2.4`
#'   translates to a line of `#SBATCH --mem-per-cpu=2.4G`
#'   in the SLURM job script.
#'   `slurm_memory_gigabytes_per_cpu = NULL` omits this line.
#' @param slurm_cpus_per_task Optional positive integer of length 1,
#'   number of CPUs for the worker.
#'   `slurm_cpus_per_task = 4` translates
#'   to a line of `#SBATCH --cpus-per-task=4` in the SLURM job script.
#'   `slurm_cpus_per_task = NULL` omits this line.
crew_launcher_slurm <- function(
  name = NULL,
  seconds_launch = 60,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 1,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  verbose = FALSE,
  command_submit = as.character(Sys.which("sbatch")),
  command_delete = as.character(Sys.which("scancel")),
  script_directory = tempdir(),
  script_lines = character(0L),
  slurm_log_output = "/dev/null",
  slurm_log_error = "/dev/null",
  slurm_memory_gigabytes_per_cpu = NULL,
  slurm_cpus_per_task = NULL
) {
  name <- as.character(name %|||% crew::crew_random_name())
  launcher <- crew_class_launcher_slurm$new(
    name = name,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    verbose = verbose,
    command_submit = command_submit,
    command_delete = command_delete,
    script_directory = script_directory,
    script_lines = script_lines,
    slurm_log_output = slurm_log_output,
    slurm_log_error = slurm_log_error,
    slurm_memory_gigabytes_per_cpu = slurm_memory_gigabytes_per_cpu,
    slurm_cpus_per_task = slurm_cpus_per_task
  )
  launcher$validate()
  launcher
}

#' @title `r lifecycle::badge("experimental")` SLURM launcher class
#' @export
#' @family launchers
#' @description `R6` class to launch and manage SLURM workers.
#' @details See [crew_launcher_slurm()].
#' @inheritSection crew.cluster-package Attribution
crew_class_launcher_slurm <- R6::R6Class(
  classname = "crew_class_launcher_slurm",
  inherit = crew_class_launcher_cluster,
  cloneable = FALSE,
  public = list(
    #' @field slurm_log_output See [crew_launcher_slurm()].
    slurm_log_output = NULL,
    #' @field slurm_log_error See [crew_launcher_slurm()].
    slurm_log_error = NULL,
    #' @field slurm_memory_gigabytes_per_cpu See [crew_launcher_slurm()].
    slurm_memory_gigabytes_per_cpu = NULL,
    #' @field slurm_cpus_per_task See [crew_launcher_slurm()].
    slurm_cpus_per_task = NULL,
    #' @description SLURM launcher constructor.
    #' @return an SLURM launcher object.
    #' @param name See [crew_launcher_slurm()].
    #' @param seconds_launch See [crew_launcher_slurm()].
    #' @param seconds_idle See [crew_launcher_slurm()].
    #' @param seconds_wall See [crew_launcher_slurm()].
    #' @param seconds_exit See [crew_launcher_slurm()].
    #' @param tasks_max See [crew_launcher_slurm()].
    #' @param tasks_timers See [crew_launcher_slurm()].
    #' @param reset_globals See [crew_launcher_slurm()].
    #' @param reset_packages See [crew_launcher_slurm()].
    #' @param reset_options See [crew_launcher_slurm()].
    #' @param garbage_collection See [crew_launcher_slurm()].
    #' @param verbose See [crew_launcher_slurm()].
    #' @param command_submit See [crew_launcher_sge()].
    #' @param command_delete See [crew_launcher_sge()].
    #' @param script_directory See [crew_launcher_sge()].
    #' @param script_lines See [crew_launcher_sge()].
    #' @param slurm_log_output See [crew_launcher_slurm()].
    #' @param slurm_log_error See [crew_launcher_slurm()].
    #' @param slurm_memory_gigabytes_per_cpu See [crew_launcher_slurm()].
    #' @param slurm_cpus_per_task See [crew_launcher_slurm()].
    initialize = function(
      name = NULL,
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      seconds_exit = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      verbose = NULL,
      command_submit = NULL,
      command_delete = NULL,
      script_directory = NULL,
      script_lines = NULL,
      slurm_log_output = NULL,
      slurm_log_error = NULL,
      slurm_memory_gigabytes_per_cpu = NULL,
      slurm_cpus_per_task = NULL
    ) {
      super$initialize(
        name = name,
        seconds_launch = seconds_launch,
        seconds_idle = seconds_idle,
        seconds_wall = seconds_wall,
        seconds_exit = seconds_exit,
        tasks_max = tasks_max,
        tasks_timers = tasks_timers,
        reset_globals = reset_globals,
        reset_packages = reset_packages,
        reset_options = reset_options,
        garbage_collection = garbage_collection,
        verbose = verbose,
        command_submit = command_submit,
        command_delete = command_delete,
        script_directory = script_directory,
        script_lines = script_lines
      )
      self$slurm_log_output <- slurm_log_output
      self$slurm_log_error <- slurm_log_error
      self$slurm_memory_gigabytes_per_cpu <- slurm_memory_gigabytes_per_cpu
      self$slurm_cpus_per_task <- slurm_cpus_per_task
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      super$validate()
      fields <- c("slurm_log_output", "slurm_log_error")
      for (field in fields) {
        if (!is.null(self[[field]])) {
          crew::crew_assert(
            self[[field]],
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
        "slurm_memory_gigabytes_per_cpu",
        "slurm_cpus_per_task"
      )
      for (field in fields) {
        if (!is.null(self[[field]])) {
          crew::crew_assert(
            self[[field]],
            is.numeric(.),
            length(.) == 1L,
            !anyNA(.),
            . > 0L,
            message = paste("invalid", field, "field")
          )
        }
      }
      invisible()
    },
    #' @description Generate the job script.
    #' @details Includes everything except the worker-instance-specific
    #'   job name and the worker-instance-specific
    #'   call to `crew::crew_worker()`, both of which get inserted at
    #'   the bottom of the script at launch time.
    #' @return Character vector of the lines of the job script.
    #' @param name Character of length 1, name of the job. For inspection
    #'   purposes, you can supply a mock job name.
    #' @examples
    #' launcher <- crew_launcher_slurm(
    #'   slurm_log_output = "log_file_%A.log",
    #'   slurm_log_error = NULL,
    #'   slurm_memory_gigabytes_per_cpu = 4096
    #' )
    #' launcher$script(name = "my_job_name")
    script = function(name) {
      c(
        "#!/bin/sh",
        paste0("#SBATCH --job-name=", name),
        if_any(
          is.null(self$slurm_log_output),
          character(0L),
          paste0("#SBATCH --output=", self$slurm_log_output)
        ),
        if_any(
          is.null(self$slurm_log_error),
          character(0L),
          paste0("#SBATCH --error=", self$slurm_log_error)
        ),
        if_any(
          is.null(self$slurm_memory_gigabytes_per_cpu),
          character(0L),
          sprintf(
            "#SBATCH --mem-per-cpu=%sG",
            self$slurm_memory_gigabytes_per_cpu
          )
        ),
        if_any(
          is.null(self$slurm_cpus_per_task),
          character(0L),
          paste0("#SBATCH --cpus-per-task=", self$slurm_cpus_per_task)
        ),
        self$script_lines
      )
    },
    #' @description Worker termination arguments.
    #' @return Character vector of arguments to the command that
    #'   terminates a worker.
    #' @param name Character of length 1, name of the job of the worker
    #'   on the scheduler.
    args_terminate = function(name) {
      c("--name", shQuote(name))
    }
  )
)
