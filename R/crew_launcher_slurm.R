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
#' @inheritParams crew::crew_launcher
#' @param verbose Logical, whether to see console output and error messages
#'   when submitting worker.
#' @param slurm_sbatch Character of length 1,
#'   file path to the `sbatch` executable
#'   used to submit `crew` workers as SLURM jobs.
#' @param slurm_scancel Character of length 1,
#'   file path to the `scancel` executable
#'   used to delete the SLURM jobs running `crew` workers.
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
#' @param slurm_memory_megabytes_per_cpu Positive numeric of length 1
#'   with the megabytes of memory required.
#'   `slurm_memory_megabytes_per_cpu = 4096`
#'   translates to a line of `#SBATCH --mem-per-cpu=4096`
#'   in the SLURM job script.
#'   `slurm_memory_megabytes_per_cpu = NULL` omits this line.
#' @param slurm_cpus_per_task Optional positive integer of length 1,
#'   number of CPUs for the worker.
#'   `slurm_cpus_per_task = 4` translates
#'   to a line of `#SBATCH --cpus-per-task=4` in the SLURM job script.
#'   `slurm_cpus_per_task = NULL` omits this line.
#' @param slurm_lines Optional character vector of additional lines to be
#'   added to the SLURM job script just after the more common flags.
#'   An example would be `slurm_lines = "module load R"` if your SLURM cluster
#'   supports R through an environment module.
crew_launcher_slurm <- function(
  name = NULL,
  verbose = FALSE,
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
  slurm_sbatch = as.character(Sys.which("sbatch")),
  slurm_scancel = as.character(Sys.which("scancel")),
  slurm_log_output = "/dev/null",
  slurm_log_error = "/dev/null",
  slurm_memory_megabytes_per_cpu = NULL,
  slurm_cpus_per_task = NULL,
  slurm_lines = NULL
) {
  name <- as.character(name %|||% crew::crew_random_name())
  launcher <- crew_class_launcher_slurm$new(
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
    slurm_sbatch = slurm_sbatch,
    slurm_scancel = slurm_scancel,
    slurm_log_output = slurm_log_output,
    slurm_log_error = slurm_log_error,
    slurm_memory_megabytes_per_cpu = slurm_memory_megabytes_per_cpu,
    slurm_cpus_per_task = slurm_cpus_per_task,
    slurm_lines = slurm_lines
  )
  launcher$validate()
  launcher
}

#' @title SLURM launcher class
#' @export
#' @family launchers
#' @description `R6` class to launch and manage SLURM workers.
#' @details See [crew_launcher_slurm()].
crew_class_launcher_slurm <- R6::R6Class(
  classname = "crew_class_launcher_slurm",
  inherit = crew::crew_class_launcher,
  cloneable = FALSE,
  public = list(
    #' @field verbose See [crew_launcher_slurm()].
    verbose = NULL,
    #' @field slurm_sbatch See [crew_launcher_slurm()].
    slurm_sbatch = NULL,
    #' @field slurm_scancel See [crew_launcher_slurm()].
    slurm_scancel = NULL,
    #' @field slurm_log_output See [crew_launcher_slurm()].
    slurm_log_output = NULL,
    #' @field slurm_log_error See [crew_launcher_slurm()].
    slurm_log_error = NULL,
    #' @field slurm_memory_megabytes_per_cpu See [crew_launcher_slurm()].
    slurm_memory_megabytes_per_cpu = NULL,
    #' @field slurm_cpus_per_task See [crew_launcher_slurm()].
    slurm_cpus_per_task = NULL,
    #' @field slurm_lines See [crew_launcher_slurm()].
    slurm_lines = NULL,
    #' @field prefix Unique prefix of worker scripts.
    prefix = NULL,
    #' @description SLURM launcher constructor.
    #' @return an SLURM launcher object.
    #' @param name See [crew_launcher_slurm()].
    #' @param seconds_launch See [crew_launcher_slurm()].
    #' @param seconds_interval See [crew_launcher_slurm()].
    #' @param seconds_timeout See [crew_launcher_slurm()].
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
    #' @param slurm_sbatch See [crew_launcher_slurm()].
    #' @param slurm_scancel See [crew_launcher_slurm()].
    #' @param slurm_log_output See [crew_launcher_slurm()].
    #' @param slurm_log_error See [crew_launcher_slurm()].
    #' @param slurm_memory_megabytes_per_cpu See [crew_launcher_slurm()].
    #' @param slurm_cpus_per_task See [crew_launcher_slurm()].
    #' @param slurm_lines See [crew_launcher_slurm()].
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
      slurm_sbatch = NULL,
      slurm_scancel = NULL,
      slurm_log_output = NULL,
      slurm_log_error = NULL,
      slurm_memory_megabytes_per_cpu = NULL,
      slurm_cpus_per_task = NULL,
      slurm_lines = NULL
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
        garbage_collection = garbage_collection
      )
      self$verbose <- verbose
      self$slurm_sbatch <- slurm_sbatch
      self$slurm_scancel <- slurm_scancel
      self$slurm_log_output <- slurm_log_output
      self$slurm_log_error <- slurm_log_error
      self$slurm_memory_megabytes_per_cpu <- slurm_memory_megabytes_per_cpu
      self$slurm_cpus_per_task <- slurm_cpus_per_task
      self$slurm_lines <- slurm_lines
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      super$validate()
      fields <- c("slurm_sbatch", "slurm_scancel")
      for (field in fields) {
        crew::crew_assert(
          self[[field]],
          is.character(.),
          length(.) == 1L,
          !anyNA(.),
          message = "slurm_scancel and slurm_sbatch must be valid character strings."
        )
      }
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
      if (!is.null(self$slurm_lines)) {
        crew::crew_assert(
          self$slurm_lines,
          is.character(.),
          !anyNA(.),
          message = "invalid slurm_lines field"
        )
      }
      crew::crew_assert(
        self$verbose,
        isTRUE(.) || isFALSE(.),
        message = "the \"verbose\" argument must be a length-1 logical."
      )
      fields <- c(
        "slurm_memory_megabytes_per_cpu",
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
    #' @description Launch a local process worker which will
    #'   dial into a socket.
    #' @details The `call` argument is R code that will run to
    #'   initiate the worker. Together, the `launcher`, `worker`,
    #'   and `instance` arguments are useful for
    #'   constructing informative job names.
    #' @return A handle object to allow the termination of the worker
    #'   later on.
    #' @param call Text string with a namespaced call to [crew_worker()]
    #'   which will run in the worker and accept tasks.
    #' @param launcher Character of length 1, name of the launcher.
    #' @param worker Positive integer of length 1, index of the worker.
    #'   This worker index remains the same even when the current instance
    #'   of the worker exits and a new instance launches.
    #'   It is always between 1 and the maximum number of concurrent workers.
    #' @param instance Character of length 1 to uniquely identify
    #'   the current instance of the worker.
    launch_worker = function(call, launcher, worker, instance) {
      name <- name_job(
        launcher = launcher,
        worker = worker,
        instance = instance
      )
      lines <- c(
        self$script(),
        paste0("#SBATCH --job-name=", name),
        paste("R -e", shQuote(call))
      )
      self$prefix <- self$prefix %|||% crew::crew_random_name()
      script <- name_script(
        prefix = self$prefix,
        launcher = launcher,
        worker = worker
      )
      writeLines(text = lines, con = script)
      system2(
        command = self$slurm_sbatch,
        args = shQuote(script),
        stdout = if_any(self$verbose, "", FALSE),
        stderr = if_any(self$verbose, "", FALSE),
        wait = FALSE
      )
      list(
        launcher = launcher,
        worker = worker,
        instance = instance
      )
    },
    #' @description Terminate a local process worker.
    #' @return `NULL` (invisibly).
    #' @param handle A process handle object previously
    #'   returned by `launch_worker()`.
    terminate_worker = function(handle) {
      script <- name_script(
        prefix = self$prefix,
        launcher = handle$launcher,
        worker = handle$worker
      )
      unlink(script)
      name <- name_job(
        launcher = handle$launcher,
        worker = handle$worker,
        instance = handle$instance
      )
      system2(
        command = self$slurm_scancel,
        args = shQuote(name),
        stdout = if_any(self$verbose, "", FALSE),
        stderr = if_any(self$verbose, "", FALSE),
        wait = FALSE
      )
      invisible()
    },
    #' @description Generate the job script.
    #' @details Includes everything except the worker-instance-specific
    #'   job name and the worker-instance-specific
    #'   call to `crew::crew_worker()`, both of which get inserted at
    #'   the bottom of the script at launch time.
    #' @return Character vector of the lines of the job script.
    #' @examples
    #' launcher <- crew_launcher_slurm(
    #'   slurm_log_output = "log_file_%A.log",
    #'   slurm_log_error = NULL,
    #'   slurm_memory_megabytes_per_cpu = 4096
    #' )
    #' launcher$script()
    script = function() {
      c(
        "#!/bin/sh",
        if_any(
          is.null(self$slurm_log_output),
          character(0L),
          paste0("$SBATCH --output=", self$slurm_log_output)
        ),
        if_any(
          is.null(self$slurm_log_error),
          character(0L),
          paste0("$SBATCH --error=", self$slurm_log_error)
        ),
        if_any(
          is.null(self$slurm_memory_megabytes_per_cpu),
          character(0L),
          paste0("$SBATCH --mem-per-cpu=", self$slurm_memory_megabytes_per_cpu)
        ),
        if_any(
          is.null(self$slurm_cpus_per_task),
          character(0L),
          paste0("$SBATCH --cpus-per-task=", self$slurm_cpus_per_task)
        ),
        self$slurm_lines
      )
    }
  )
)
