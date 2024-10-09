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
#' @param options_cluster An options list from
#'   [crew_options_lsf()] with cluster-specific configuration options.
#' @param lsf_cwd Deprecated. Use `options_cluster` instead.
#' @param lsf_log_output Deprecated. Use `options_cluster` instead.
#' @param lsf_log_error Deprecated. Use `options_cluster` instead.
#' @param lsf_memory_gigabytes_limit Deprecated.
#'   Use `options_cluster` instead.
#' @param lsf_memory_gigabytes_required Deprecated.
#'   Use `options_cluster` instead.
#' @param lsf_cores Deprecated. Use `options_cluster` instead.
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
  r_arguments = c("--no-save", "--no-restore"),
  options_metrics = crew::crew_options_metrics(),
  options_cluster = crew.cluster::crew_options_lsf(),
  verbose = NULL,
  command_submit = NULL,
  command_terminate = NULL,
  command_delete = NULL,
  script_directory = NULL,
  script_lines = NULL,
  lsf_cwd = NULL,
  lsf_log_output = NULL,
  lsf_log_error = NULL,
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
  deprecated <- c(
    "verbose",
    "command_submit",
    "command_terminate",
    "command_delete",
    "script_directory",
    "script_lines",
    "lsf_cwd",
    "lsf_log_output",
    "lsf_log_error",
    "lsf_memory_gigabytes_limit",
    "lsf_memory_gigabytes_required",
    "lsf_cores"
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
    field <- gsub("^lsf_", "", arg)
    options_cluster[[field]] <- value %|||% options_cluster[[field]]
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
    r_arguments = r_arguments,
    options_metrics = options_metrics,
    options_cluster = options_cluster
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
    .args_launch = function(script) {
      c("<", shQuote(script))
    },
    .args_terminate = function(name) {
      c("-J", shQuote(name))
    }
  ),
  public = list(
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      crew::crew_assert(
        private$.options_cluster,
        inherits(., "crew_options_lsf"),
        message = "crew_options must be a crew_options_lsf() object."
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
          is.null(private$.options_cluster$cwd),
          character(0L),
          paste("#BSUB -cwd", private$.options_cluster$cwd)
        ),
        if_any(
          is.null(private$.options_cluster$log_output),
          character(0L),
          paste("#BSUB -o", private$.options_cluster$log_output)
        ),
        if_any(
          is.null(private$.options_cluster$log_error),
          character(0L),
          paste("#BSUB -e", private$.options_cluster$log_error)
        ),
        if_any(
          is.null(private$.options_cluster$memory_gigabytes_limit),
          character(0L),
          sprintf(
            "#BSUB -M %sG",
            private$.options_cluster$memory_gigabytes_limit
          )
        ),
        if_any(
          is.null(private$.options_cluster$memory_gigabytes_required),
          character(0L),
          sprintf(
            "#BSUB -R 'rusage[mem=%sG]'",
            private$.options_cluster$memory_gigabytes_required
          )
        ),
        if_any(
          is.null(private$.options_cluster$cores),
          character(0L),
          paste("#BSUB -n", private$.options_cluster$cores)
        ),
        private$.options_cluster$script_lines
      )
    }
  )
)
