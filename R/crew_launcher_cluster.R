#' @title `r lifecycle::badge("maturing")` Create an abstract cluster
#'   launcher object.
#' @export
#' @family launchers
#' @description Create an `R6` abstract cluster launcher object.
#' @details This abstract class is used to develop specific launcher classes
#'   for specific computing platforms.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew::crew_launcher
#' @param verbose Logical, whether to see console output and error messages
#'   when submitting worker.
#' @param command_submit Character of length 1,
#'   file path to the executable to submit a worker job.
#' @param command_delete Character of length 1,
#'   file path to the executable to delete a worker job.
#'   Set to `""` to skip manually terminating the worker.
#'   Unless there is an issue with the platform,
#'   the job should still exit thanks to the NNG-powered network programming
#'   capabilities of `mirai`. Still, if you set `command_delete = ""`,
#'   you are assuming extra responsibility for manually monitoring
#'   your jobs on the cluster and manually terminating jobs as appropriate.
#' @param script_directory Character of length 1, directory path to the
#'   job scripts. Just before each job submission, a job script
#'   is created in this folder. Script base names are unique to each
#'   launcher and worker, and the launcher deletes the script when the
#'   worker is manually terminated. `tempdir()` is the default, but it
#'   might not work for some systems.
#'   `tools::R_user_dir("crew.cluster", which = "cache")`
#'   is another reasonable choice.
#' @param script_lines Optional character vector of additional lines to be
#'   added to the job script just after the more common flags.
#'   An example would be `script_lines = "module load R"` if your cluster
#'   supports R through an environment module.
crew_launcher_cluster <- function(
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
  verbose = FALSE,
  command_submit = "",
  command_delete = "",
  script_directory = tempdir(),
  script_lines = character(0L)
) {
  name <- as.character(name %|||% crew::crew_random_name())
  launcher <- crew_class_launcher_cluster$new(
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
    verbose = verbose,
    command_submit = command_submit,
    command_delete = command_delete,
    script_directory = script_directory,
    script_lines = script_lines
  )
  launcher$validate()
  launcher
}

#' @title `r lifecycle::badge("maturing")` Abstract cluster launcher class
#' @export
#' @family launchers
#' @description `R6` class to help develop specific cluster launcher plugins.
#' @details See [crew_launcher_cluster()].
#' @inheritSection crew.cluster-package Attribution
crew_class_launcher_cluster <- R6::R6Class(
  classname = "crew_class_launcher_cluster",
  inherit = crew::crew_class_launcher,
  cloneable = FALSE,
  public = list(
    #' @field verbose See [crew_launcher_cluster()].
    verbose = NULL,
    #' @field command_submit See [crew_launcher_cluster()].
    command_submit = NULL,
    #' @field command_delete See [crew_launcher_cluster()].
    command_delete = NULL,
    #' @field script_directory See [crew_launcher_cluster()].
    script_directory = NULL,
    #' @field script_lines See [crew_launcher_cluster()].
    script_lines = NULL,
    #' @field prefix See [crew_launcher_cluster()].
    prefix = NULL,
    #' @description Abstract launcher constructor.
    #' @return An abstract launcher object.
    #' @param name See [crew_launcher_cluster()].
    #' @param seconds_interval See [crew_launcher_cluster()].
    #' @param seconds_launch See [crew_launcher_cluster()].
    #' @param seconds_idle See [crew_launcher_cluster()].
    #' @param seconds_wall See [crew_launcher_cluster()].
    #' @param seconds_exit See [crew_launcher_cluster()].
    #' @param tasks_max See [crew_launcher_cluster()].
    #' @param tasks_timers See [crew_launcher_cluster()].
    #' @param reset_globals See [crew_launcher_cluster()].
    #' @param reset_packages See [crew_launcher_cluster()].
    #' @param reset_options See [crew_launcher_cluster()].
    #' @param garbage_collection See [crew_launcher_cluster()].
    #' @param verbose See [crew_launcher_cluster()].
    #' @param command_submit See [crew_launcher_cluster()].
    #' @param command_delete See [crew_launcher_cluster()].
    #' @param script_directory See [crew_launcher_cluster()].
    #' @param script_lines See [crew_launcher_cluster()].
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
      verbose = NULL,
      command_submit = NULL,
      command_delete = NULL,
      script_directory = NULL,
      script_lines = NULL
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
        garbage_collection = garbage_collection
      )
      self$verbose <- verbose
      self$command_submit <- command_submit
      self$command_delete <- command_delete
      self$script_directory <- script_directory
      self$script_lines <- script_lines
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      super$validate() # nolint
      crew::crew_assert(
        self$verbose,
        isTRUE(.) || isFALSE(.),
        message = "the \"verbose\" field is not a length-1 logical."
      )
      fields <- c("command_submit", "command_submit", "script_directory")
      for (field in fields) {
        crew::crew_assert(
          self[[field]],
          is.character(.),
          length(.) == 1L,
          !anyNA(.),
          message = paste(field, "must be a valid length-1 character string.")
        )
      }
      if (!is.null(self$script_lines)) {
        crew::crew_assert(
          self$script_lines,
          is.character(.),
          !anyNA(.),
          message = "invalid script_lines field"
        )
      }
      invisible()
    },
    #' @description Launch a local process worker which will
    #'   dial into a socket.
    #' @details The `call` argument is R code that will run to
    #'   initiate the worker.
    #' @return A handle object to allow the termination of the worker
    #'   later on.
    #' @param call Character of length 1, a namespaced call to [crew_worker()]
    #'   which will run in the worker and accept tasks.
    #' @param name Character of length 1, an informative worker name.
    #' @param launcher Character of length 1, name of the launcher.
    #' @param worker Positive integer of length 1, index of the worker.
    #'   This worker index remains the same even when the current instance
    #'   of the worker exits and a new instance launches.
    #'   It is always between 1 and the maximum number of concurrent workers.
    #' @param instance Character of length 1 to uniquely identify
    #'   the current instance of the worker.
    launch_worker = function(call, name, launcher, worker, instance) {
      lines <- c(self$script(name = name), paste("R -e", shQuote(call)))
      if (is.null(self$prefix)) {
        if (!file.exists(self$script_directory)) {
          dir.create(self$script_directory, recursive = TRUE)
        }
        self$prefix <- crew::crew_random_name()
      }
      script <- path_script(
        dir = self$script_directory,
        prefix = self$prefix,
        launcher = launcher,
        worker = worker
      )
      writeLines(text = lines, con = script)
      system2(
        command = self$command_submit,
        args = self$args_launch(script = script),
        stdout = if_any(self$verbose, "", FALSE),
        stderr = if_any(self$verbose, "", FALSE),
        wait = FALSE
      )
      list(name = name, script = script)
    },
    #' @description Terminate a local process worker.
    #' @return `NULL` (invisibly).
    #' @param handle A process handle object previously
    #'   returned by `launch_worker()`.
    terminate_worker = function(handle) {
      unlink(handle$script)
      if (nzchar(self$command_delete)) {
        system2(
          command = self$command_delete,
          args = self$args_terminate(name = handle$name),
          stdout = if_any(self$verbose, "", FALSE),
          stderr = if_any(self$verbose, "", FALSE),
          wait = FALSE
        )
      }
      invisible()
    },
    #' @description Worker launch arguments.
    #' @return Character vector of arguments to the command that
    #'   launches a worker.
    #' @param script Character of length 1, path to the job script for
    #'   the scheduler.
    args_launch = function(script) {
      shQuote(script)
    },
    #' @description Worker termination arguments.
    #' @return Character vector of arguments to the command that
    #'   terminates a worker.
    #' @param name Character of length 1, name of the job of the worker
    #'   on the scheduler.
    args_terminate = function(name) {
      shQuote(name)
    }
  )
)
