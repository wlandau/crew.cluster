#' @title `r lifecycle::badge('experimental')` Create a launcher with
#'   LSF workers.
#' @export
#' @family launchers
#' @description Create an `R6` object to launch and maintain
#'   workers as LSF jobs.
#' @details WARNING: the `crew.cluster` LSF plugin is experimental
#'   and has not actually been tested on a LSF cluster. Please proceed
#'   with caution and report bugs to
#'   <https://github.com/wlandau/crew.cluster>.
#'
#'   To launch a LSF worker, this launcher
#'   creates a temporary job script with a call to `crew::crew_worker()`
#'   and submits it as an LSF job with `sbatch`. To see most of the lines
#'   of the job script in advance, use the `script()` method of the launcher.
#'   It has all the lines except for the job name and the
#'   call to `crew::crew_worker()`, both of
#'   which will be inserted at the last minute when it is time
#'   to actually launch a worker.
#' @inheritParams crew_launcher_cluster
#' @param lsf_log_output Character of length 1, file pattern to control
#'   the locations of the LSF worker log files. By default, both standard
#'   output and standard error go to the same file.
#'   `lsf_log_output = "crew_log_%J.log"` translates to a line of
#'   `#BSUB-o crew_log_%J.log` in the LSF job script,
#'   where `%J` is replaced by the job ID of the worker.
#'   The default is `/dev/null` to omit these logs.
#'   Set `lsf_log_output = NULL` to omit this line from the job script.
#' @param lsf_log_error Character of length 1, file pattern for standard
#'   error. `lsf_log_error = "crew_error_%J.err"` translates to a line of
#'   `#BSUB-e crew_error_%J.err` in the LSF job script,
#'   where `%J` is replaced by the job ID of the worker.
#'   The default is `/dev/null` to omit these logs.
#'   Set `lsf_log_error = NULL` to omit this line from the job script.
#' @param lsf_memory_megabytes_per_cpu Positive numeric of length 1
#'   with the megabytes of memory required.
#'   `lsf_memory_megabytes_per_cpu = 4096`
#'   translates to a line of `#BSUB-M 4096`
#'   in the LSF job script.
#'   `lsf_memory_megabytes_per_cpu = NULL` omits this line.
#' @param lsf_cpus_per_task Optional positive integer of length 1,
#'   number of CPUs for the worker.
#'   `lsf_cpus_per_task = 4` translates
#'   to a line of `#BSUB-n 4` in the LSF job script.
#'   `lsf_cpus_per_task = NULL` omits this line.
crew_launcher_lsf <- function(
  name = NULL,
  seconds_launch = 60,
  seconds_interval = 0.001,
  seconds_timeout = 10,
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
  command_submit = as.character(Sys.which("bsub")),
  command_delete = as.character(Sys.which("bkill")),
  script_directory = tempdir(),
  script_lines = character(0L),
  lsf_log_output = "/dev/null",
  lsf_log_error = "/dev/null",
  lsf_memory_megabytes_per_cpu = NULL,
  lsf_cpus_per_task = NULL
) {
  name <- as.character(name %|||% crew::crew_random_name())
  launcher <- crew_class_launcher_lsf$new(
    name = name,
    seconds_launch = seconds_launch,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
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
    lsf_log_output = lsf_log_output,
    lsf_log_error = lsf_log_error,
    lsf_memory_megabytes_per_cpu = lsf_memory_megabytes_per_cpu,
    lsf_cpus_per_task = lsf_cpus_per_task
  )
  launcher$validate()
  launcher
}

#' @title `r lifecycle::badge("experimental")` LSF launcher class
#' @export
#' @family launchers
#' @description `R6` class to launch and manage LSF workers.
#' @details See [crew_launcher_lsf()].
crew_class_launcher_lsf <- R6::R6Class(
  classname = "crew_class_launcher_lsf",
  inherit = crew_class_launcher_cluster,
  cloneable = FALSE,
  public = list(
    #' @field lsf_log_output See [crew_launcher_lsf()].
    lsf_log_output = NULL,
    #' @field lsf_log_error See [crew_launcher_lsf()].
    lsf_log_error = NULL,
    #' @field lsf_memory_megabytes_per_cpu See [crew_launcher_lsf()].
    lsf_memory_megabytes_per_cpu = NULL,
    #' @field lsf_cpus_per_task See [crew_launcher_lsf()].
    lsf_cpus_per_task = NULL,
    #' @description LSF launcher constructor.
    #' @return an LSF launcher object.
    #' @param name See [crew_launcher_lsf()].
    #' @param seconds_launch See [crew_launcher_lsf()].
    #' @param seconds_interval See [crew_launcher_lsf()].
    #' @param seconds_timeout See [crew_launcher_lsf()].
    #' @param seconds_idle See [crew_launcher_lsf()].
    #' @param seconds_wall See [crew_launcher_lsf()].
    #' @param seconds_exit See [crew_launcher_lsf()].
    #' @param tasks_max See [crew_launcher_lsf()].
    #' @param tasks_timers See [crew_launcher_lsf()].
    #' @param reset_globals See [crew_launcher_lsf()].
    #' @param reset_packages See [crew_launcher_lsf()].
    #' @param reset_options See [crew_launcher_lsf()].
    #' @param garbage_collection See [crew_launcher_lsf()].
    #' @param verbose See [crew_launcher_lsf()].
    #' @param command_submit See [crew_launcher_sge()].
    #' @param command_delete See [crew_launcher_sge()].
    #' @param script_directory See [crew_launcher_sge()].
    #' @param script_lines See [crew_launcher_sge()].
    #' @param lsf_log_output See [crew_launcher_lsf()].
    #' @param lsf_log_error See [crew_launcher_lsf()].
    #' @param lsf_memory_megabytes_per_cpu See [crew_launcher_lsf()].
    #' @param lsf_cpus_per_task See [crew_launcher_lsf()].
    initialize = function(
      name = NULL,
      seconds_launch = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
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
      lsf_log_output = NULL,
      lsf_log_error = NULL,
      lsf_memory_megabytes_per_cpu = NULL,
      lsf_cpus_per_task = NULL
    ) {
      super$initialize(
        name = name,
        seconds_launch = seconds_launch,
        seconds_interval = seconds_interval,
        seconds_timeout = seconds_timeout,
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
      self$lsf_log_output <- lsf_log_output
      self$lsf_log_error <- lsf_log_error
      self$lsf_memory_megabytes_per_cpu <- lsf_memory_megabytes_per_cpu
      self$lsf_cpus_per_task <- lsf_cpus_per_task
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      super$validate()
      fields <- c("lsf_log_output", "lsf_log_error")
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
        "lsf_memory_megabytes_per_cpu",
        "lsf_cpus_per_task"
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
    #' launcher <- crew_launcher_lsf(
    #'   lsf_log_output = "log_file_%J.log",
    #'   lsf_log_error = NULL,
    #'   lsf_memory_megabytes_per_cpu = 4096
    #' )
    #' launcher$script(name = "my_job_name")
    script = function(name) {
      c(
        "#!/bin/sh",
        paste("#BSUB-J", name),
        if_any(
          is.null(self$lsf_log_output),
          character(0L),
          paste("#BSUB-o", self$lsf_log_output)
        ),
        if_any(
          is.null(self$lsf_log_error),
          character(0L),
          paste("#BSUB-e", self$lsf_log_error)
        ),
        if_any(
          is.null(self$lsf_memory_megabytes_per_cpu),
          character(0L),
          paste("#BSUB-M", self$lsf_memory_megabytes_per_cpu)
        ),
        if_any(
          is.null(self$lsf_cpus_per_task),
          character(0L),
          paste("#BSUB-n", self$lsf_cpus_per_task)
        ),
        self$script_lines
      )
    },
    #' @description Worker launch arguments.
    #' @return Character vector of arguments to the command that
    #'   launches a worker.
    #' @param script Character of length 1, path to the job script for
    #'   the scheduler.
    args_launch = function(script) {
      c("<", shQuote(script))
    },
    #' @description Termination arguments.
    #' @return Character vector of arguments to the command that
    #'   terminates a worker.
    #' @param name Character of length 1, name of the job of the worker
    #'   on the scheduler.
    args_terminate = function(name) {
      c("-J", shQuote(name))
    }
  )
)
