#' @title Create a controller with a Sun Grid Engine (SGE) launcher.
#' @export
#' @family controllers
#' @description Create an `R6` object to submit tasks and
#'   launch workers on Sun Grid Engine (SGE) workers.
#' @inheritParams crew::crew_router
#' @inheritParams crew_launcher_sge
#' @inheritParams crew::crew_controller
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
  sge_qsub = as.character(Sys.which("qsub")),
  sge_qdel = as.character(Sys.which("qdel")),
  sge_cwd = TRUE,
  sge_envvars = FALSE,
  sge_log_files = "/dev/null",
  sge_log_join = TRUE,
  sge_memory_gigabytes_required = NULL,
  sge_memory_gigabytes_limit = NULL,
  sge_cores = NULL,
  sge_gpu = NULL,
  sge_lines = NULL,
  auto_scale = "demand"
) {
  router <- crew::crew_router(
    name = name,
    workers = workers,
    host = host,
    port = port,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- crew_launcher_sge(
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
    sge_qsub = sge_qsub,
    sge_qdel = sge_qdel,
    sge_cwd = sge_cwd,
    sge_envvars = sge_envvars,
    sge_log_files = sge_log_files,
    sge_log_join = sge_log_join,
    sge_memory_gigabytes_required = sge_memory_gigabytes_required,
    sge_memory_gigabytes_limit = sge_memory_gigabytes_limit,
    sge_cores = sge_cores,
    sge_gpu = sge_gpu,
    sge_lines = sge_lines
  )
  controller <- crew::crew_controller(
    router = router,
    launcher = launcher,
    auto_scale = auto_scale
  )
  controller$validate()
  controller
}
