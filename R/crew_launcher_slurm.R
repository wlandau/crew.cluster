#' @title `r lifecycle::badge("maturing")` Create a launcher with
#'   SLURM workers.
#' @export
#' @family slurm
#' @description Create an `R6` object to launch and maintain
#'   workers as SLURM jobs.
#' @details To launch a SLURM worker, this launcher
#'   creates a temporary job script with a call to `crew::crew_worker()`
#'   and submits it as an SLURM job with `sbatch`. To see most of the lines
#'   of the job script in advance, use the `script()` method of the launcher.
#'   It has all the lines except for the job name and the
#'   call to `crew::crew_worker()`, both of
#'   which will be inserted at the last minute when it is time
#'   to actually launch a worker.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew_launcher_cluster
#' @param options_cluster An options list from
#'   [crew_options_slurm()] with cluster-specific configuration options.
#' @param slurm_log_output Deprecated. Use `options_cluster` instead.
#' @param slurm_log_error Deprecated. Use `options_cluster` instead.
#' @param slurm_memory_gigabytes_required Deprecated.
#'   Use `options_cluster` instead.
#' @param slurm_memory_gigabytes_per_cpu Deprecated.
#'   Use `options_cluster` instead.
#' @param slurm_cpus_per_task Deprecated. Use `options_cluster` instead.
#' @param slurm_time_minutes Deprecated. Use `options_cluster` instead.
#' @param slurm_partition Deprecated. Use `options_cluster` instead.
crew_launcher_slurm <- function(
  name = NULL,
  workers = 1L,
  seconds_interval = 0.5,
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
  options_cluster = crew.cluster::crew_options_slurm(),
  verbose = NULL,
  command_submit = NULL,
  command_terminate = NULL,
  command_delete = NULL,
  script_directory = NULL,
  script_lines = NULL,
  slurm_log_output = NULL,
  slurm_log_error = NULL,
  slurm_memory_gigabytes_required = NULL,
  slurm_memory_gigabytes_per_cpu = NULL,
  slurm_cpus_per_task = NULL,
  slurm_time_minutes = NULL,
  slurm_partition = NULL
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
    "slurm_log_output",
    "slurm_log_error",
    "slurm_memory_gigabytes_required",
    "slurm_memory_gigabytes_per_cpu",
    "slurm_cpus_per_task",
    "slurm_time_minutes",
    "slurm_partition"
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
    field <- gsub("^slurm_", "", arg)
    options_cluster[[field]] <- value %|||% options_cluster[[field]]
  }
  launcher <- crew_class_launcher_slurm$new(
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
    .args_terminate = function(name) {
      c("--name", shQuote(name))
    }
  ),
  public = list(
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      crew::crew_assert(
        private$.options_cluster,
        inherits(., "crew_options_slurm"),
        message = "crew_options must be a crew_options_slurm() object."
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
    #' launcher <- crew_launcher_slurm(
    #'   slurm_log_output = "log_file_%A.log",
    #'   slurm_log_error = NULL,
    #'   slurm_memory_gigabytes_per_cpu = 4096
    #' )
    #' launcher$script(name = "my_job_name")
    #' }
    script = function(name) {
      options <- private$.options_cluster
      c(
        "#!/bin/sh",
        paste0("#SBATCH --job-name=", name),
        if_any(
          is.null(options$log_output),
          character(0L),
          paste0("#SBATCH --output=", options$log_output)
        ),
        if_any(
          is.null(options$log_error),
          character(0L),
          paste0("#SBATCH --error=", options$log_error)
        ),
        if_any(
          is.null(options$memory_gigabytes_required),
          character(0L),
          sprintf(
            "#SBATCH --mem=%sM",
            as.integer(
              round(
                x = 1024 * options$memory_gigabytes_required,
                digits = 0L
              )
            )
          )
        ),
        if_any(
          is.null(options$memory_gigabytes_per_cpu),
          character(0L),
          sprintf(
            "#SBATCH --mem-per-cpu=%sM",
            as.integer(
              round(
                x = 1024 * options$memory_gigabytes_per_cpu,
                digits = 0L
              )
            )
          )
        ),
        if_any(
          is.null(options$cpus_per_task),
          character(0L),
          paste0(
            "#SBATCH --cpus-per-task=",
            options$cpus_per_task
          )
        ),
        if_any(
          is.null(options$time_minutes),
          character(0L),
          paste0("#SBATCH --time=", options$time_minutes)
        ),
        if_any(
          is.null(options$partition),
          character(0L),
          paste0("#SBATCH --partition=", options$partition)
        ),
        if_any(
          is.null(options$n_tasks),
          character(0L),
          paste0("#SBATCH --ntasks=", options$n_tasks)
        ),
        options$script_lines
      )
    }
  )
)
