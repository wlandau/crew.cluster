#' @title `lifecycle::badge("experimental")` Create a controller with a
#'   SLURM launcher.
#' @export
#' @family controllers
#' @description Create an `R6` object to submit tasks and
#'   launch workers on SLURM workers.
#' @details WARNING: the `crew.cluster` SLURM plugin is experimental
#'   and has not actually been tested on a SLURM cluster. Please proceed
#'   with caution and report bugs to
#'   <https://github.com/wlandau/crew.cluster>.
#' @inheritParams crew::crew_router
#' @inheritParams crew_launcher_slurm
#' @inheritParams crew::crew_controller
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
  seconds_launch = 60,
  seconds_interval = 0.01,
  seconds_timeout = 5,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 0.1,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  verbose = FALSE,
  slurm_sbatch = as.character(Sys.which("sbatch")),
  slurm_scancel = as.character(Sys.which("scancel")),
  slurm_log_output = "/dev/null",
  slurm_log_error = "/dev/null",
  slurm_memory_megabytes_per_cpu = NULL,
  slurm_cpus_per_task = NULL,
  slurm_lines = NULL
) {
  router <- crew::crew_router(
    name = name,
    workers = workers,
    host = host,
    port = port,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- crew_launcher_slurm(
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
  controller <- crew::crew_controller(
    router = router,
    launcher = launcher,
    auto_scale = auto_scale
  )
  controller$validate()
  controller
}