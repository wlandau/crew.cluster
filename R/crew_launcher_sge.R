#' @title `r lifecycle::badge("maturing")` Create a launcher with
#'   Sun Grid Engine (SGE) workers.
#' @export
#' @family launchers
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
#' @param sge_cwd Logical of length 1, whether to
#'   launch the worker from the current working directory (as opposed to
#'   the user home directory). `sge_cwd = TRUE` translates to a line of
#'   `#$ -cwd` in the SGE job script. `sge_cwd = FALSE` omits this line.
#' @param sge_envvars Logical of length 1, whether to forward the environment
#'   variables of the current session to the SGE worker. `sge_envvars = TRUE`
#'   translates to a line of `#$ -V` in the SGE job script.
#'   `sge_envvars = FALSE` omits this line.
#' @param sge_log_output Character of length 1, file or directory path to SGE
#'   worker log files for standard output.
#'   `sge_log_output = "VALUE"` translates to a line of
#'   `#$ -o VALUE` in the SGE job script. The default is `/dev/null` to omit
#'   the logs. If you do supply a non-`/dev/null` value,
#'   it is recommended to supply a
#'   directory path with a trailing slash so that each worker gets its own set
#'   of log files.
#' @param sge_log_error Character of length 1, file or directory path to SGE
#'   worker log files for standard error.
#'   `sge_log_error = "VALUE"` translates to a line of
#'   `#$ -e VALUE` in the SGE job script.
#'   The default of `NULL` omits this line.
#'   If you do supply a non-`/dev/null` value, it is recommended to supply a
#'   directory path with a trailing slash so that each worker gets its own set
#'   of log files.
#' @param sge_log_join Logical, whether to join the stdout and stderr log
#'   files together into one file. `sge_log_join = TRUE` translates to a line
#'   of `#$ -j y` in the SGE job script, while `sge_log_join = FALSE` is
#'   equivalent to `#$ -j n`. If `sge_log_join = TRUE`, then `sge_log_error`
#'   should be `NULL`.
#' @param sge_memory_gigabytes_limit Optional numeric of length 1
#'   with the maximum number of gigabytes of memory a worker is allowed to
#'   consume. If the worker consumes more than this level of memory, then
#'   SGE will terminate it. `sge_memory_gigabytes_limit = 5.7"`
#'   translates to a line of `"#$ -l h_rss=5.7G"` in the SGE job script.
#'   `sge_memory_gigabytes_limit = NULL` omits this line.
#' @param sge_memory_gigabytes_required Optional positive numeric of length 1
#'   with the gigabytes of memory required to run the worker.
#'   `sge_memory_gigabytes_required = 2.4`
#'   translates to a line of `#$ -l m_mem_free=2.4G` in the SGE job script.
#'   `sge_memory_gigabytes_required = NULL` omits this line.
#' @param sge_cores Optional positive integer of length 1,
#'   number of cores per worker ("slots" in SGE lingo).
#'   `sge_cores = 4` translates
#'   to a line of `#$ -pe smp 4` in the SGE job script.
#'   `sge_cores = NULL` omits this line.
#' @param sge_gpu Optional integer of length 1 with the number of GPUs to
#'   request for the worker. `sge_gpu = 1` translates to a line of
#'   `"#$ -l gpu=1"` in the SGE job script. `sge_gpu = NULL` omits this line.
crew_launcher_sge <- function(
  name = NULL,
  seconds_interval = 0.25,
  seconds_launch = 86400,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 1,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  launch_max = 5L,
  tls = crew::crew_tls(),
  verbose = FALSE,
  command_submit = as.character(Sys.which("qsub")),
  command_delete = as.character(Sys.which("qdel")),
  script_directory = tempdir(),
  script_lines = character(0L),
  sge_cwd = TRUE,
  sge_envvars = FALSE,
  sge_log_output = "/dev/null",
  sge_log_error = NULL,
  sge_log_join = TRUE,
  sge_memory_gigabytes_limit = NULL,
  sge_memory_gigabytes_required = NULL,
  sge_cores = NULL,
  sge_gpu = NULL
) {
  name <- as.character(name %|||% crew::crew_random_name())
  launcher <- crew_class_launcher_sge$new(
    name = name,
    seconds_interval = seconds_interval,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
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
  launcher$validate()
  launcher
}

#' @title `r lifecycle::badge("maturing")` SGE launcher class
#' @export
#' @family launchers
#' @description `R6` class to launch and manage SGE workers.
#' @details See [crew_launcher_sge()].
#' @inheritSection crew.cluster-package Attribution
crew_class_launcher_sge <- R6::R6Class(
  classname = "crew_class_launcher_sge",
  inherit = crew_class_launcher_cluster,
  cloneable = FALSE,
  public = list(
    #' @field sge_cwd See [crew_launcher_sge()].
    sge_cwd = NULL,
    #' @field sge_envvars See [crew_launcher_sge()].
    sge_envvars = NULL,
    #' @field sge_log_output See [crew_launcher_sge()].
    sge_log_output = NULL,
    #' @field sge_log_error See [crew_launcher_sge()].
    sge_log_error = NULL,
    #' @field sge_log_join See [crew_launcher_sge()].
    sge_log_join = NULL,
    #' @field sge_memory_gigabytes_limit See [crew_launcher_sge()].
    sge_memory_gigabytes_limit = NULL,
    #' @field sge_memory_gigabytes_required See [crew_launcher_sge()].
    sge_memory_gigabytes_required = NULL,
    #' @field sge_cores See [crew_launcher_sge()].
    sge_cores = NULL,
    #' @field sge_gpu See [crew_launcher_sge()].
    sge_gpu = NULL,
    #' @description SGE launcher constructor.
    #' @return an SGE launcher object.
    #' @param name See [crew_launcher_sge()].
    #' @param seconds_interval See [crew_launcher_sge()].
    #' @param seconds_launch See [crew_launcher_sge()].
    #' @param seconds_idle See [crew_launcher_sge()].
    #' @param seconds_wall See [crew_launcher_sge()].
    #' @param seconds_exit See [crew_launcher_sge()].
    #' @param tasks_max See [crew_launcher_sge()].
    #' @param tasks_timers See [crew_launcher_sge()].
    #' @param reset_globals See [crew_launcher_sge()].
    #' @param reset_packages See [crew_launcher_sge()].
    #' @param reset_options See [crew_launcher_sge()].
    #' @param garbage_collection See [crew_launcher_sge()].
    #' @param launch_max See [crew_launcher_sge()].
    #' @param tls See [crew_launcher_sge()].
    #' @param verbose See [crew_launcher_sge()].
    #' @param command_submit See [crew_launcher_sge()].
    #' @param command_delete See [crew_launcher_sge()].
    #' @param script_directory See [crew_launcher_sge()].
    #' @param script_lines See [crew_launcher_sge()].
    #' @param sge_cwd See [crew_launcher_sge()].
    #' @param sge_envvars See [crew_launcher_sge()].
    #' @param sge_log_output See [crew_launcher_sge()].
    #' @param sge_log_error See [crew_launcher_sge()].
    #' @param sge_log_join See [crew_launcher_sge()].
    #' @param sge_memory_gigabytes_limit See [crew_launcher_sge()].
    #' @param sge_memory_gigabytes_required See [crew_launcher_sge()].
    #' @param sge_cores See [crew_launcher_sge()].
    #' @param sge_gpu See [crew_launcher_sge()].
    initialize = function(
      name = NULL,
      seconds_interval = NULL,
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      seconds_exit = NULL,
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
      super$initialize(
        name = name,
        seconds_interval = seconds_interval,
        seconds_launch = seconds_launch,
        seconds_idle = seconds_idle,
        seconds_wall = seconds_wall,
        seconds_exit = seconds_exit,
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
        script_lines = script_lines
      )
      self$sge_cwd <- sge_cwd
      self$sge_envvars <- sge_envvars
      self$sge_log_output <- sge_log_output
      self$sge_log_error <- sge_log_error
      self$sge_log_join <- sge_log_join
      self$sge_memory_gigabytes_limit <- sge_memory_gigabytes_limit
      self$sge_memory_gigabytes_required <- sge_memory_gigabytes_required
      self$sge_cores <- sge_cores
      self$sge_gpu <- sge_gpu
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      super$validate()
      crew::crew_assert(
        self$sge_log_output,
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        message = "sge_log_output must be a nonempty length-1 character string."
      )
      if (!is.null(self$sge_log_error)) {
        crew::crew_assert(
          self$sge_log_error,
          is.character(.),
          length(.) == 1L,
          !anyNA(.),
          nzchar(.),
          message = paste(
            "sge_log_error must be a nonempty",
            "length-1 character string."
          )
        )
      }
      fields <- c(
        "sge_cwd",
        "sge_envvars",
        "sge_log_join"
      )
      for (field in fields) {
        crew::crew_assert(
          self[[field]],
          isTRUE(.) || isFALSE(.),
          message = paste(field, "is not a length-1 logical.")
        )
      }
      fields <- c(
        "sge_memory_gigabytes_limit",
        "sge_memory_gigabytes_required",
        "sge_cores",
        "sge_gpu"
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
    #' launcher <- crew_launcher_sge(
    #'   sge_cores = 2,
    #'   sge_memory_gigabytes_required = 4
    #' )
    #' launcher$script(name = "my_job_name")
    #' }
    script = function(name) {
      c(
        paste("#$ -N", name),
        if_any(self$sge_cwd, "#$ -cwd", character(0L)),
        if_any(self$sge_envvars, "#$ -V", character(0L)),
        paste("#$ -o", self$sge_log_output),
        if_any(
          is.null(self$sge_log_error),
          character(0L),
          paste("#$ -e", self$sge_log_error)
        ),
        if_any(self$sge_log_join, "#$ -j y", "#$ -j n"),
        if_any(
          is.null(self$sge_memory_gigabytes_limit),
          character(0L),
          sprintf("#$ -l h_rss=%sG", self$sge_memory_gigabytes_limit)
        ),
        if_any(
          is.null(self$sge_memory_gigabytes_required),
          character(0L),
          sprintf("#$ -l m_mem_free=%sG", self$sge_memory_gigabytes_required)
        ),
        if_any(
          is.null(self$sge_cores),
          character(0L),
          paste("#$ -pe smp", as.character(self$sge_cores))
        ),
        if_any(
          is.null(self$sge_gpu),
          character(0L),
          paste0("#$ -l gpu=", as.character(self$sge_gpu))
        ),
        self$script_lines
      )
    }
  )
)
