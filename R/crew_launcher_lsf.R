#' @title `r lifecycle::badge('experimental')` Create a launcher with
#'   LSF workers.
#' @export
#' @family lsf
#' @description Create an `R6` object to launch and maintain
#'   workers as LSF jobs.
#' @details WARNING: the `crew.cluster` LSF plugin is experimental.
#'   Please proceed with caution and report bugs to
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
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew_launcher_cluster
#' @param lsf_cwd Character of length 1, directory to
#'   launch the worker from (as opposed to
#'   the system default). `lsf_cwd = "/home"` translates to a line of
#'   `#BSUB -cwd /home` in the LSF job script. `lsf_cwd = getwd()` is the
#'   default, which launches workers from the current working directory.
#'   Set `lsf_cwd = NULL` to omit this line from the job script.
#' @param lsf_log_output Character of length 1, file pattern to control
#'   the locations of the LSF worker log files. By default, both standard
#'   output and standard error go to the same file.
#'   `lsf_log_output = "crew_log_%J.log"` translates to a line of
#'   `#BSUB -o crew_log_%J.log` in the LSF job script,
#'   where `%J` is replaced by the job ID of the worker.
#'   The default is `/dev/null` to omit these logs.
#'   Set `lsf_log_output = NULL` to omit this line from the job script.
#' @param lsf_log_error Character of length 1, file pattern for standard
#'   error. `lsf_log_error = "crew_error_%J.err"` translates to a line of
#'   `#BSUB -e crew_error_%J.err` in the LSF job script,
#'   where `%J` is replaced by the job ID of the worker.
#'   The default is `/dev/null` to omit these logs.
#'   Set `lsf_log_error = NULL` to omit this line from the job script.
#' @param lsf_memory_gigabytes_limit Positive numeric of length 1
#'   with the limit in gigabytes
#'   `lsf_memory_gigabytes_limit = 4`
#'   translates to a line of `#BSUB -M 4G`
#'   in the LSF job script.
#'   `lsf_memory_gigabytes_limit = NULL` omits this line.
#' @param lsf_memory_gigabytes_required Positive numeric of length 1
#'   with the memory requirement in gigabytes
#'   `lsf_memory_gigabytes_required = 4`
#'   translates to a line of `#BSUB -R 'rusage[mem=4G]'`
#'   in the LSF job script.
#'   `lsf_memory_gigabytes_required = NULL` omits this line.
#' @param lsf_cores Optional positive integer of length 1,
#'   number of CPU cores for the worker.
#'   `lsf_cores = 4` translates
#'   to a line of `#BSUB -n 4` in the LSF job script.
#'   `lsf_cores = NULL` omits this line.
crew_launcher_lsf <- function(
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
  command_submit = as.character(Sys.which("bsub")),
  command_terminate = as.character(Sys.which("bkill")),
  command_delete = NULL,
  script_directory = tempdir(),
  script_lines = character(0L),
  lsf_cwd = getwd(),
  lsf_log_output = "/dev/null",
  lsf_log_error = "/dev/null",
  lsf_memory_gigabytes_limit = NULL,
  lsf_memory_gigabytes_required = NULL,
  lsf_cores = NULL
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
  launcher <- crew_class_launcher_lsf$new(
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
    lsf_cwd = lsf_cwd,
    lsf_log_output = lsf_log_output,
    lsf_log_error = lsf_log_error,
    lsf_memory_gigabytes_limit = lsf_memory_gigabytes_limit,
    lsf_memory_gigabytes_required = lsf_memory_gigabytes_required,
    lsf_cores = lsf_cores
  )
  launcher$validate()
  launcher
}

#' @title `r lifecycle::badge("experimental")` LSF launcher class
#' @export
#' @family lsf
#' @description `R6` class to launch and manage LSF workers.
#' @details See [crew_launcher_lsf()].
#' @inheritSection crew.cluster-package Attribution
crew_class_launcher_lsf <- R6::R6Class(
  classname = "crew_class_launcher_lsf",
  inherit = crew_class_launcher_cluster,
  cloneable = FALSE,
  private = list(
    .lsf_cwd = NULL,
    .lsf_log_output = NULL,
    .lsf_log_error = NULL,
    .lsf_memory_gigabytes_limit = NULL,
    .lsf_memory_gigabytes_required = NULL,
    .lsf_cores = NULL,
    .args_launch = function(script) {
      c("<", shQuote(script))
    },
    .args_terminate = function(name) {
      c("-J", shQuote(name))
    }
  ),
  active = list(
    #' @field lsf_cwd See [crew_launcher_lsf()].
    lsf_cwd = function() {
      .subset2(private, ".lsf_cwd")
    },
    #' @field lsf_log_output See [crew_launcher_lsf()].
    lsf_log_output = function() {
      .subset2(private, ".lsf_log_output")
    },
    #' @field lsf_log_error See [crew_launcher_lsf()].
    lsf_log_error = function() {
      .subset2(private, ".lsf_log_error")
    },
    #' @field lsf_memory_gigabytes_limit See [crew_launcher_lsf()].
    lsf_memory_gigabytes_limit = function() {
      .subset2(private, ".lsf_memory_gigabytes_limit")
    },
    #' @field lsf_memory_gigabytes_required See [crew_launcher_lsf()].
    lsf_memory_gigabytes_required = function() {
      .subset2(private, ".lsf_memory_gigabytes_required")
    },
    #' @field lsf_cores See [crew_launcher_lsf()].
    lsf_cores = function() {
      .subset2(private, ".lsf_cores")
    }
  ),
  public = list(
    #' @description LSF launcher constructor.
    #' @return an LSF launcher object.
    #' @param name See [crew_launcher_lsf()].
    #' @param seconds_interval See [crew_launcher_lsf()].
    #' @param seconds_timeout See [crew_launcher_lsf()].
    #' @param seconds_launch See [crew_launcher_lsf()].
    #' @param seconds_idle See [crew_launcher_lsf()].
    #' @param seconds_wall See [crew_launcher_lsf()].
    #' @param tasks_max See [crew_launcher_lsf()].
    #' @param tasks_timers See [crew_launcher_lsf()].
    #' @param reset_globals See [crew_launcher_lsf()].
    #' @param reset_packages See [crew_launcher_lsf()].
    #' @param reset_options See [crew_launcher_lsf()].
    #' @param garbage_collection See [crew_launcher_lsf()].
    #' @param launch_max See [crew_launcher_lsf()].
    #' @param tls See [crew_launcher_lsf()].
    #' @param verbose See [crew_launcher_lsf()].
    #' @param command_submit See [crew_launcher_lsf()].
    #' @param command_terminate See [crew_launcher_lsf()].
    #' @param script_directory See [crew_launcher_lsf()].
    #' @param script_lines See [crew_launcher_lsf()].
    #' @param lsf_cwd See [crew_launcher_lsf()].
    #' @param lsf_log_output See [crew_launcher_lsf()].
    #' @param lsf_log_error See [crew_launcher_lsf()].
    #' @param lsf_memory_gigabytes_limit See [crew_launcher_lsf()].
    #' @param lsf_memory_gigabytes_required See [crew_launcher_lsf()].
    #' @param lsf_cores See [crew_launcher_lsf()].
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
      lsf_cwd = NULL,
      lsf_log_output = NULL,
      lsf_log_error = NULL,
      lsf_memory_gigabytes_limit = NULL,
      lsf_memory_gigabytes_required = NULL,
      lsf_cores = NULL
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
      private$.lsf_cwd <- lsf_cwd
      private$.lsf_log_output <- lsf_log_output
      private$.lsf_log_error <- lsf_log_error
      private$.lsf_memory_gigabytes_limit <- lsf_memory_gigabytes_limit
      private$.lsf_memory_gigabytes_required <- lsf_memory_gigabytes_required
      private$.lsf_cores <- lsf_cores
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      super$validate()
      fields <- c("lsf_log_output", "lsf_log_error", "lsf_cwd")
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
        "lsf_memory_gigabytes_limit",
        "lsf_memory_gigabytes_required",
        "lsf_cores"
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
    #' launcher <- crew_launcher_lsf(
    #'   lsf_cwd = getwd(),
    #'   lsf_log_output = "log_file_%J.log",
    #'   lsf_log_error = NULL,
    #'   lsf_memory_gigabytes_limit = 4
    #' )
    #' launcher$script(name = "my_job_name")
    #' }
    script = function(name) {
      c(
        "#!/bin/sh",
        paste("#BSUB -J", name),
        if_any(
          is.null(private$.lsf_cwd),
          character(0L),
          paste("#BSUB -cwd", private$.lsf_cwd)
        ),
        if_any(
          is.null(private$.lsf_log_output),
          character(0L),
          paste("#BSUB -o", private$.lsf_log_output)
        ),
        if_any(
          is.null(private$.lsf_log_error),
          character(0L),
          paste("#BSUB -e", private$.lsf_log_error)
        ),
        if_any(
          is.null(private$.lsf_memory_gigabytes_limit),
          character(0L),
          sprintf("#BSUB -M %sG", private$.lsf_memory_gigabytes_limit)
        ),
        if_any(
          is.null(private$.lsf_memory_gigabytes_required),
          character(0L),
          sprintf(
            "#BSUB -R 'rusage[mem=%sG]'",
            private$.lsf_memory_gigabytes_required
          )
        ),
        if_any(
          is.null(private$.lsf_cores),
          character(0L),
          paste("#BSUB -n", private$.lsf_cores)
        ),
        private$.script_lines
      )
    }
  )
)
