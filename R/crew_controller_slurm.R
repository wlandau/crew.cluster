#' @title `r lifecycle::badge("experimental")` Create a controller with a
#'   SLURM launcher.
#' @export
#' @family controllers
#' @description Create an `R6` object to submit tasks and
#'   launch workers on SLURM workers.
#' @details WARNING: the `crew.cluster` SLURM plugin is experimental
#'   and has not actually been tested on a SLURM cluster. Please proceed
#'   with caution and report bugs to
#'   <https://github.com/wlandau/crew.cluster>.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew::crew_client
#' @inheritParams crew_launcher_slurm
#' @inheritParams crew::crew_controller
#' @param seconds_exit Deprecated on 2023-09-21 in version 0.1.2.9000.
#'   No longer necessary.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' controller <- crew_controller_slurm()
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()$result
#' controller$terminate()
#' }
crew_controller_slurm <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  port = NULL,
  tls = crew::crew_tls(mode = "automatic"),
  tls_enable = NULL,
  tls_config = NULL,
  seconds_interval = 0.25,
  seconds_timeout = 10,
  seconds_launch = 86400,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = NULL,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  launch_max = 5L,
  verbose = FALSE,
  command_submit = as.character(Sys.which("sbatch")),
  command_delete = as.character(Sys.which("scancel")),
  script_directory = tempdir(),
  script_lines = character(0L),
  slurm_log_output = "/dev/null",
  slurm_log_error = "/dev/null",
  slurm_memory_gigabytes_per_cpu = NULL,
  slurm_cpus_per_task = NULL,
  slurm_time_minutes = 1440,
  slurm_partition = NULL
) {
  if (!is.null(seconds_exit)) {
    crew::crew_deprecate(
      name = "seconds_exit",
      date = "2023-09-21",
      version = "0.5.0.9002",
      alternative = "none (no longer necessary)"
    )
  }
  client <- crew::crew_client(
    name = name,
    workers = workers,
    host = host,
    port = port,
    tls = tls,
    tls_enable = tls_enable,
    tls_config = tls_config,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- crew_launcher_slurm(
    name = name,
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
    command_delete = command_delete,
    script_directory = script_directory,
    script_lines = script_lines,
    slurm_log_output = slurm_log_output,
    slurm_log_error = slurm_log_error,
    slurm_memory_gigabytes_per_cpu = slurm_memory_gigabytes_per_cpu,
    slurm_cpus_per_task = slurm_cpus_per_task,
    slurm_time_minutes = slurm_time_minutes,
    slurm_partition = slurm_partition
  )
  controller <- crew::crew_controller(client = client, launcher = launcher)
  controller$validate()
  controller
}
