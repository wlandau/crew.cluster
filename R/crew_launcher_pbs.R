#' @title `r lifecycle::badge("experimental")` Create a launcher with
#'   PBS or TORQUE workers.
#' @export
#' @family pbs
#' @description Create an `R6` object to launch and maintain
#'   workers as jobs on a PBS or TORQUE cluster.
#' @details WARNING: the `crew.cluster` PBS plugin is experimental
#'   and has not actually been tested on a PBS cluster. Please proceed
#'   with caution and report bugs to
#'   <https://github.com/wlandau/crew.cluster>.
#'
#'   To launch a PBS/TORQUE worker, this launcher
#'   creates a temporary job script with a call to `crew::crew_worker()`
#'   and submits it as an PBS job with `qsub`. To see most of the lines
#'   of the job script in advance, use the `script()` method of the launcher.
#'   It has all the lines except for the job name and the
#'   call to `crew::crew_worker()`, both of
#'   which will be inserted at the last minute when it is time
#'   to actually launch a worker.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew_launcher_cluster
#' @param options_cluster An options list from
#'   [crew_options_pbs()] with cluster-specific configuration options.
#' @param pbs_cwd Deprecated. Use `options_cluster` instead.
#' @param pbs_log_output Deprecated. Use `options_cluster` instead.
#' @param pbs_log_error Deprecated. Use `options_cluster` instead.
#' @param pbs_log_join Deprecated. Use `options_cluster` instead.
#' @param pbs_memory_gigabytes_required Deprecated.
#'   Use `options_cluster` instead.
#' @param pbs_cores Deprecated. Use `options_cluster` instead.
#' @param pbs_walltime_hours Deprecated. Use `options_cluster` instead.
crew_launcher_pbs <- function(
  name = NULL,
  workers = 1L,
  seconds_interval = 0.5,
  seconds_timeout = 60,
  seconds_launch = 86400,
  seconds_idle = 300,
  seconds_wall = Inf,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  crashes_error = NULL,
  tls = crew::crew_tls(mode = "automatic"),
  r_arguments = c("--no-save", "--no-restore"),
  options_metrics = crew::crew_options_metrics(),
  options_cluster = crew.cluster::crew_options_pbs(),
  verbose = NULL,
  command_submit = NULL,
  command_terminate = NULL,
  command_delete = NULL,
  script_directory = NULL,
  script_lines = NULL,
  pbs_cwd = NULL,
  pbs_log_output = NULL,
  pbs_log_error = NULL,
  pbs_log_join = NULL,
  pbs_memory_gigabytes_required = NULL,
  pbs_cores = NULL,
  pbs_walltime_hours = NULL
) {
  name <- as.character(name %|||% crew::crew_random_name())
  crew::crew_deprecate(
    name = "crashes_error",
    date = "2025-01-27",
    version = "0.3.4",
    alternative = "crashes_error",
    condition = "message",
    value = crashes_error
  )
  if (!is.null(command_delete)) {
    crew::crew_deprecate(
      name = "command_delete",
      date = "2023-01-08",
      version = "0.1.4.9001",
      alternative = "command_terminate"
    )
    command_terminate <- command_delete
  }
  deprecated <- c(
    "verbose",
    "command_submit",
    "command_terminate",
    "script_directory",
    "script_lines",
    "pbs_cwd",
    "pbs_log_output",
    "pbs_log_error",
    "pbs_log_join",
    "pbs_memory_gigabytes_required",
    "pbs_cores",
    "pbs_walltime_hours"
  )
  for (arg in deprecated) {
    value <- get(arg)
    crew::crew_deprecate(
      name = arg,
      date = "2024-10-09",
      version = "0.3.2.9005",
      alternative = "options_cluster argument",
      value = value
    )
    field <- gsub("^pbs_", "", arg)
    options_cluster[[field]] <- value %|||% options_cluster[[field]]
  }
  launcher <- crew_class_launcher_pbs$new(
    name = name,
    workers = workers,
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
    crashes_error = crashes_error,
    tls = tls,
    r_arguments = r_arguments,
    options_metrics = options_metrics,
    options_cluster = options_cluster
  )
  launcher$validate()
  launcher
}

#' @title `r lifecycle::badge("maturing")` PBS/TORQUE launcher class
#' @export
#' @family pbs
#' @description `R6` class to launch and manage PBS/TORQUE workers.
#' @details See [crew_launcher_pbs()].
#' @inheritSection crew.cluster-package Attribution
crew_class_launcher_pbs <- R6::R6Class(
  classname = "crew_class_launcher_pbs",
  inherit = crew_class_launcher_cluster,
  cloneable = FALSE,
  public = list(
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      crew::crew_assert(
        private$.options_cluster,
        inherits(., "crew_options_pbs"),
        message = "crew_options must be a crew_options_pbs() object."
      )
      super$validate() # nolint
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
    #' launcher <- crew_launcher_pbs(
    #'   pbs_cores = 2,
    #'   pbs_memory_gigabytes_required = 4
    #' )
    #' launcher$script(name = "my_job_name")
    #' }
    script = function(name) {
      options <- private$.options_cluster
      c(
        paste("#PBS -N", name),
        paste("#PBS -o", options$log_output),
        if_any(
          is.null(options$log_error),
          character(0L),
          paste("#PBS -e", options$log_error)
        ),
        if_any(options$log_join, "#PBS -j oe", "#PBS -j n"),
        if_any(
          is.null(options$memory_gigabytes_required),
          character(0L),
          sprintf(
            "#PBS -l mem=%sgb",
            options$memory_gigabytes_required
          )
        ),
        if_any(
          is.null(options$cores),
          character(0L),
          paste0("#PBS -l ppn=", as.character(options$cores))
        ),
        if_any(
          is.null(options$walltime_hours),
          character(0L),
          sprintf(
            "#PBS -l walltime=%s:00:00",
            as.character(options$walltime_hours)
          )
        ),
        options$script_lines,
        if_any(
          options$cwd,
          "cd \"$PBS_O_WORKDIR\"",
          character(0L)
        )
      )
    }
  )
)
