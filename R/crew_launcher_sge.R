#' @title `r lifecycle::badge("maturing")` Create a launcher with
#'   Sun Grid Engine (SGE) workers.
#' @export
#' @family sge
#' @description Create an `R6` object to launch and maintain
#'   workers as Sun Grid Engine (SGE) jobs.
#' @details To launch a Sun Grid Engine (SGE) worker, this launcher
#'   creates a temporary job script with a call to `crew::crew_worker()`
#'   and submits it as an SGE job with `qsub`. To see most of the lines
#'   of the job script in advance, use the `script()` method of the launcher.
#'   It has all the lines except for the job name and the
#'   call to `crew::crew_worker()`, both of
#'   which will be inserted at the last minute when it is time
#'   to actually launch a worker.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew_launcher_cluster
#' @param options_cluster An options list from
#'   [crew_options_sge()] with cluster-specific configuration options.
#' @param sge_cwd Deprecated. Use `options_cluster` instead.
#' @param sge_envvars Deprecated. Use `options_cluster` instead.
#' @param sge_log_output Deprecated. Use `options_cluster` instead.
#' @param sge_log_error Deprecated. Use `options_cluster` instead.
#' @param sge_log_join Deprecated. Use `options_cluster` instead.
#' @param sge_memory_gigabytes_limit Deprecated.
#'   Use `options_cluster` instead.
#' @param sge_memory_gigabytes_required Deprecated.
#'   Use `options_cluster` instead.
#' @param sge_cores Deprecated. Use `options_cluster` instead.
#' @param sge_gpu Deprecated. Use `options_cluster` instead.
crew_launcher_sge <- function(
  name = NULL,
  workers = 1L,
  seconds_interval = 0.25,
  seconds_timeout = 60,
  seconds_launch = 86400,
  seconds_idle = 300,
  seconds_wall = Inf,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = NULL,
  reset_packages = NULL,
  reset_options = NULL,
  garbage_collection = NULL,
  crashes_error = NULL,
  tls = crew::crew_tls(mode = "automatic"),
  r_arguments = c("--no-save", "--no-restore"),
  options_metrics = crew::crew_options_metrics(),
  options_cluster = crew.cluster::crew_options_sge(),
  verbose = NULL,
  command_submit = NULL,
  command_terminate = NULL,
  command_delete = NULL,
  script_directory = NULL,
  script_lines = NULL,
  sge_cwd = NULL,
  sge_envvars = NULL,
  sge_log_output = NULL,
  sge_log_error = NULL,
  sge_log_join = NULL,
  sge_memory_gigabytes_limit = NULL,
  sge_memory_gigabytes_required = NULL,
  sge_cores = NULL,
  sge_gpu = NULL
) {
  name <- as.character(name %|||% crew::crew_random_name())
  crew::crew_deprecate(
    name = "crashes_error",
    date = "2025-01-27",
    version = "0.3.4",
    alternative = "crashes_max",
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
    "command_delete",
    "script_directory",
    "script_lines",
    "sge_cwd",
    "sge_envvars",
    "sge_log_output",
    "sge_log_error",
    "sge_log_join",
    "sge_memory_gigabytes_limit",
    "sge_memory_gigabytes_required",
    "sge_cores",
    "sge_gpu"
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
    field <- gsub("^sge_", "", arg)
    options_cluster[[field]] <- value %|||% options_cluster[[field]]
  }
  launcher <- crew_class_launcher_sge$new(
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
    tls = tls,
    r_arguments = r_arguments,
    options_metrics = options_metrics,
    options_cluster = options_cluster
  )
  launcher$validate()
  launcher
}

#' @title `r lifecycle::badge("maturing")` SGE launcher class
#' @export
#' @family sge
#' @description `R6` class to launch and manage SGE workers.
#' @details See [crew_launcher_sge()].
#' @inheritSection crew.cluster-package Attribution
crew_class_launcher_sge <- R6::R6Class(
  classname = "crew_class_launcher_sge",
  inherit = crew_class_launcher_cluster,
  cloneable = FALSE,
  public = list(
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      crew::crew_assert(
        private$.options_cluster,
        inherits(., "crew_options_sge"),
        message = "crew_options must be a crew_options_sge() object."
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
    #' @param n Positive integer of length 1, number of crew workers
    #'   (i.e. cluster jobs) to launch in the current round
    #'   of auto-scaling.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' launcher <- crew_launcher_sge(
    #'   sge_cores = 2,
    #'   sge_memory_gigabytes_required = 4
    #' )
    #' launcher$script(name = "my_job_name")
    #' }
    script = function(name, n) {
      options <- private$.options_cluster
      c(
        paste("#$ -N", name),
        paste0("#$ -t 1-", n),
        if_any(options$cwd, "#$ -cwd", character(0L)),
        if_any(options$envvars, "#$ -V", character(0L)),
        paste("#$ -o", options$log_output),
        if_any(
          is.null(options$log_error),
          character(0L),
          paste("#$ -e", options$log_error)
        ),
        if_any(options$log_join, "#$ -j y", "#$ -j n"),
        if_any(
          is.null(options$memory_gigabytes_limit),
          character(0L),
          sprintf(
            "#$ -l h_rss=%sG",
            options$memory_gigabytes_limit
          )
        ),
        if_any(
          is.null(options$memory_gigabytes_required),
          character(0L),
          sprintf(
            "#$ -l m_mem_free=%sG",
            options$memory_gigabytes_required
          )
        ),
        if_any(
          is.null(options$cores),
          character(0L),
          paste("#$ -pe smp", as.character(options$cores))
        ),
        if_any(
          is.null(options$gpu),
          character(0L),
          paste0("#$ -l gpu=", as.character(options$gpu))
        ),
        options$script_lines
      )
    }
  )
)
