#' @title `r lifecycle::badge("maturing")` Create a controller with a
#'   Sun Grid Engine (SGE) launcher.
#' @export
#' @family sge
#' @description Create an `R6` object to submit tasks and
#'   launch workers on Sun Grid Engine (SGE) workers.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew::crew_controller
#' @inheritParams crew_launcher_sge
#' @inheritParams crew::crew_client
#' @param seconds_exit Deprecated on 2023-09-21 in version 0.1.2.9000.
#'   No longer necessary.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' controller <- crew_controller_sge()
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()$result
#' controller$terminate()
#' }
crew_controller_sge <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  port = NULL,
  tls = crew::crew_tls(mode = "automatic"),
  tls_enable = NULL,
  tls_config = NULL,
  serialization = NULL,
  seconds_interval = 0.25,
  seconds_timeout = 60,
  seconds_launch = 86400,
  seconds_idle = 300,
  seconds_wall = Inf,
  seconds_exit = NULL,
  retry_tasks = NULL,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  crashes_error = NULL,
  r_arguments = c("--no-save", "--no-restore"),
  crashes_max = 5L,
  backup = NULL,
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
  crew::crew_deprecate(
    name = "crashes_error",
    date = "2025-01-27",
    version = "0.3.4",
    alternative = "crashes_max",
    condition = "message",
    value = crashes_error
  )
  crew::crew_deprecate(
    name = "retry_tasks",
    date = "2025-01-27",
    version = "0.3.e4",
    alternative = "none",
    condition = "message",
    value = retry_tasks
  )
  client <- crew::crew_client(
    host = host,
    port = port,
    tls = tls,
    tls_enable = tls_enable,
    tls_config = tls_config,
    serialization = serialization,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- crew_launcher_sge(
    name = name,
    workers = workers,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    tls = tls,
    r_arguments = r_arguments,
    options_metrics = options_metrics,
    options_cluster = options_cluster,
    verbose = verbose,
    command_submit = command_submit,
    command_terminate = command_terminate,
    command_delete = command_delete,
    script_directory = script_directory,
    script_lines = script_lines,
    sge_cwd = sge_cwd,
    sge_envvars = sge_envvars,
    sge_log_output = sge_log_output,
    sge_log_error = sge_log_error,
    sge_log_join = sge_log_join,
    sge_memory_gigabytes_limit = sge_memory_gigabytes_limit,
    sge_memory_gigabytes_required = sge_memory_gigabytes_required,
    sge_cores = sge_cores,
    sge_gpu = sge_gpu
  )
  controller <- crew::crew_controller(
    client = client,
    launcher = launcher,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    crashes_max = crashes_max,
    backup = backup
  )
  controller$validate()
  controller
}
