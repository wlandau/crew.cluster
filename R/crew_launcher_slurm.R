#' @title `r lifecycle::badge('experimental')` Create a launcher with
#'   SLURM workers.
#' @export
#' @family slurm
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
#'   `slurm_memory_gigabytes_per_cpu = 2.40123`
#'   translates to a line of `#SBATCH --mem-per-cpu=2041M`
#'   in the SLURM job script.
#'   `slurm_memory_gigabytes_per_cpu = NULL` omits this line.
#' @param slurm_cpus_per_task Optional positive integer of length 1,
#'   number of CPUs for the worker.
#'   `slurm_cpus_per_task = 4` translates
#'   to a line of `#SBATCH --cpus-per-task=4` in the SLURM job script.
#'   `slurm_cpus_per_task = NULL` omits this line.
#' @param slurm_time_minutes Numeric of length 1, number of minutes to
#'   designate as the wall time of `crew` each worker instance on the
#'   SLURM cluster. `slurm_time_minutes = 60` translates to a line of
#'   `#SBATCH --time=60` in the SLURM job script. `slurm_time_minutes = NULL`
#'   omits this line.
#' @param slurm_partition Character of length 1, name of the SLURM partition to
#'   create workers on. `slurm_partition = "partition1,partition2"`
#'   translates to a line of `#SBATCH --partition=partition1,partition2`
#'   in the SLURM job script. `slurm_partition = NULL`
#'   omits this line.
crew_launcher_slurm <- function(
  name = NULL,
  seconds_interval = 0.5,
  seconds_timeout = 60,
  seconds_launch = 86400,
  seconds_idle = Inf,
  seconds_wall = Inf,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  launch_max = 5L,
  tls = crew::crew_tls(mode = "automatic"),
  verbose = FALSE,
  command_submit = as.character(Sys.which("sbatch")),
  command_terminate = as.character(Sys.which("scancel")),
  command_delete = NULL,
  script_directory = tempdir(),
  script_lines = character(0L),
  slurm_log_output = "/dev/null",
  slurm_log_error = "/dev/null",
  slurm_memory_gigabytes_per_cpu = NULL,
  slurm_cpus_per_task = NULL,
  slurm_time_minutes = 1440,
  slurm_partition = NULL
) {
  name <- as.character(name %|||% crew::crew_random_name())
  if (!is.null(command_delete)) {
    crew::crew_deprecate(
      name = "command_delete",
      date = "2023-01-08",
      version = "0.1.4.9001",
      alternative = "command_terminate"
    )
    command_terminate <- command_delete
  }
  launcher <- crew_class_launcher_slurm$new(
    name = name,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    launch_max = launch_max,
    tls = tls,
    verbose = verbose,
    command_submit = command_submit,
    command_terminate = command_terminate,
    script_directory = script_directory,
    script_lines = script_lines,
    slurm_log_output = slurm_log_output,
    slurm_log_error = slurm_log_error,
    slurm_memory_gigabytes_per_cpu = slurm_memory_gigabytes_per_cpu,
    slurm_cpus_per_task = slurm_cpus_per_task,
    slurm_time_minutes = slurm_time_minutes,
    slurm_partition = slurm_partition
  )
  launcher$validate()
  launcher
}

#' @title `r lifecycle::badge("experimental")` SLURM launcher class
#' @export
#' @family slurm
#' @description `R6` class to launch and manage SLURM workers.
#' @details See [crew_launcher_slurm()].
#' @inheritSection crew.cluster-package Attribution
crew_class_launcher_slurm <- R6::R6Class(
  classname = "crew_class_launcher_slurm",
  inherit = crew_class_launcher_cluster,
  cloneable = FALSE,
  private = list(
    .slurm_log_output = NULL,
    .slurm_log_error = NULL,
    .slurm_memory_gigabytes_per_cpu = NULL,
    .slurm_cpus_per_task = NULL,
    .slurm_time_minutes = NULL,
    .slurm_partition = NULL,
    .args_terminate = function(name) {
      c("--name", shQuote(name))
    }
  ),
  active = list(
    #' @field slurm_log_output See [crew_launcher_slurm()].
    slurm_log_output = function() {
      .subset2(private, ".slurm_log_output")
    },
    #' @field slurm_log_error See [crew_launcher_slurm()].
    slurm_log_error = function() {
      .subset2(private, ".slurm_log_error")
    },
    #' @field slurm_memory_gigabytes_per_cpu See [crew_launcher_slurm()].
    slurm_memory_gigabytes_per_cpu = function() {
      .subset2(private, ".slurm_memory_gigabytes_per_cpu")
    },
    #' @field slurm_cpus_per_task See [crew_launcher_slurm()].
    slurm_cpus_per_task = function() {
      .subset2(private, ".slurm_cpus_per_task")
    },
    #' @field slurm_time_minutes See [crew_launcher_slurm()].
    slurm_time_minutes = function() {
      .subset2(private, ".slurm_time_minutes")
    },
    #' @field slurm_partition See See [crew_launcher_slurm()].
    slurm_partition = function() {
      .subset2(private, ".slurm_partition")
    }
  ),
  public = list(
    #' @description SLURM launcher constructor.
    #' @return an SLURM launcher object.
    #' @param name See [crew_launcher_slurm()].
    #' @param seconds_interval See [crew_launcher_slurm()].
    #' @param seconds_timeout See [crew_launcher_slurm()].
    #' @param seconds_launch See [crew_launcher_slurm()].
    #' @param seconds_idle See [crew_launcher_slurm()].
    #' @param seconds_wall See [crew_launcher_slurm()].
    #' @param tasks_max See [crew_launcher_slurm()].
    #' @param tasks_timers See [crew_launcher_slurm()].
    #' @param reset_globals See [crew_launcher_slurm()].
    #' @param reset_packages See [crew_launcher_slurm()].
    #' @param reset_options See [crew_launcher_slurm()].
    #' @param garbage_collection See [crew_launcher_slurm()].
    #' @param launch_max See [crew_launcher_slurm()].
    #' @param tls See [crew_launcher_slurm()].
    #' @param verbose See [crew_launcher_slurm()].
    #' @param command_submit See [crew_launcher_sge()].
    #' @param command_terminate See [crew_launcher_sge()].
    #' @param script_directory See [crew_launcher_sge()].
    #' @param script_lines See [crew_launcher_sge()].
    #' @param slurm_log_output See [crew_launcher_slurm()].
    #' @param slurm_log_error See [crew_launcher_slurm()].
    #' @param slurm_memory_gigabytes_per_cpu See [crew_launcher_slurm()].
    #' @param slurm_cpus_per_task See [crew_launcher_slurm()].
    #' @param slurm_time_minutes See [crew_launcher_slurm()].
    #' @param slurm_partition See [crew_launcher_slurm()].
    initialize = function(
      name = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      launch_max = NULL,
      tls = NULL,
      verbose = NULL,
      command_submit = NULL,
      command_terminate = NULL,
      script_directory = NULL,
      script_lines = NULL,
      slurm_log_output = NULL,
      slurm_log_error = NULL,
      slurm_memory_gigabytes_per_cpu = NULL,
      slurm_cpus_per_task = NULL,
      slurm_time_minutes = NULL,
      slurm_partition = NULL
    ) {
      super$initialize(
        name = name,
        seconds_interval = seconds_interval,
        seconds_timeout = seconds_timeout,
        seconds_launch = seconds_launch,
        seconds_idle = seconds_idle,
        seconds_wall = seconds_wall,
        tasks_max = tasks_max,
        tasks_timers = tasks_timers,
        reset_globals = reset_globals,
        reset_packages = reset_packages,
        reset_options = reset_options,
        garbage_collection = garbage_collection,
        launch_max = launch_max,
        tls = tls,
        verbose = verbose,
        command_submit = command_submit,
        command_terminate = command_terminate,
        script_directory = script_directory,
        script_lines = script_lines
      )
      private$.slurm_log_output <- slurm_log_output
      private$.slurm_log_error <- slurm_log_error
      private$.slurm_memory_gigabytes_per_cpu <- slurm_memory_gigabytes_per_cpu
      private$.slurm_cpus_per_task <- slurm_cpus_per_task
      private$.slurm_time_minutes <- slurm_time_minutes
      private$.slurm_partition <- slurm_partition
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      super$validate()
      fields <- c("slurm_log_output", "slurm_log_error", "slurm_partition")
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
        "slurm_cpus_per_task",
        "slurm_time_minutes"
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
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' launcher <- crew_launcher_slurm(
    #'   slurm_log_output = "log_file_%A.log",
    #'   slurm_log_error = NULL,
    #'   slurm_memory_gigabytes_per_cpu = 4096
    #' )
    #' launcher$script(name = "my_job_name")
    #' }
    script = function(name) {
      c(
        "#!/bin/sh",
        paste0("#SBATCH --job-name=", name),
        if_any(
          is.null(private$.slurm_log_output),
          character(0L),
          paste0("#SBATCH --output=", private$.slurm_log_output)
        ),
        if_any(
          is.null(private$.slurm_log_error),
          character(0L),
          paste0("#SBATCH --error=", private$.slurm_log_error)
        ),
        if_any(
          is.null(private$.slurm_memory_gigabytes_per_cpu),
          character(0L),
          sprintf(
            "#SBATCH --mem-per-cpu=%sM",
            as.integer(
              round(
                x = 1024 * private$.slurm_memory_gigabytes_per_cpu,
                digits = 0L
              )
            )
          )
        ),
        if_any(
          is.null(private$.slurm_cpus_per_task),
          character(0L),
          paste0("#SBATCH --cpus-per-task=", private$.slurm_cpus_per_task)
        ),
        if_any(
          is.null(private$.slurm_time_minutes),
          character(0L),
          paste0("#SBATCH --time=", private$.slurm_time_minutes)
        ),
        if_any(
          is.null(private$.slurm_partition),
          character(0L),
          paste0("#SBATCH --partition=", private$.slurm_partition)
        ),
        private$.script_lines
      )
    }
  )
)
