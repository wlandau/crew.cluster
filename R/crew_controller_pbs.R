#' @title `r lifecycle::badge("experimental")` Create a controller with a
#'   PBS/TORQUE launcher.
#' @export
#' @family pbs
#' @description Create an `R6` object to submit tasks and
#'   launch workers on a PBS or TORQUE cluster.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew::crew_controller
#' @inheritParams crew_launcher_pbs
#' @inheritParams crew::crew_client
#' @param seconds_exit Deprecated on 2023-09-21 in version 0.1.2.9000.
#'   No longer necessary.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' controller <- crew_controller_pbs()
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()$result
#' controller$terminate()
#' }
crew_controller_pbs <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  port = NULL,
  tls = crew::crew_tls(mode = "automatic"),
  tls_enable = NULL,
  tls_config = NULL,
  serialization = NULL,
  seconds_interval = 0.5,
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
  launcher <- crew_launcher_pbs(
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
    pbs_cwd = pbs_cwd,
    pbs_log_output = pbs_log_output,
    pbs_log_error = pbs_log_error,
    pbs_log_join = pbs_log_join,
    pbs_memory_gigabytes_required = pbs_memory_gigabytes_required,
    pbs_cores = pbs_cores,
    pbs_walltime_hours = pbs_walltime_hours
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
