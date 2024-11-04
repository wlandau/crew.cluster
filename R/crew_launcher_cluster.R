#' @title `r lifecycle::badge("maturing")` Create an abstract cluster
#'   launcher object.
#' @export
#' @family cluster
#' @keywords internal
#' @description Create an `R6` abstract cluster launcher object.
#' @details This abstract class is used to develop specific launcher classes
#'   for specific computing platforms.
#' @inheritSection crew.cluster-package Attribution
#' @inheritParams crew::crew_launcher
#' @param options_cluster List of options from a `crew.cluster`
#'   options function such as [crew_options_slurm()].
#'   Make sure the cluster types of the launcher and options function match.
#' @param verbose Deprecated. Use `options_cluster` instead.
#' @param command_submit Deprecated. Use `options_cluster` instead.
#' @param command_terminate Deprecated. Use `options_cluster` instead.
#' @param command_delete Deprecated on 2024-01-08 (version 0.1.4.9001).
#'   Use `command_terminate` instead.
#' @param script_directory Deprecated. Use `options_cluster` instead.
#' @param script_lines Deprecated. Use `options_cluster` instead.
crew_launcher_cluster <- function(
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
  crashes_error = 5L,
  tls = crew::crew_tls(mode = "automatic"),
  r_arguments = c("--no-save", "--no-restore"),
  options_metrics = crew::crew_options_metrics(),
  options_cluster = crew.cluster::crew_options_cluster(),
  verbose = NULL,
  command_submit = NULL,
  command_terminate = NULL,
  command_delete = NULL,
  script_directory = NULL,
  script_lines = NULL
) {
  name <- as.character(name %|||% crew::crew_random_name())
  crew::crew_deprecate(
    name = "command_delete",
    date = "2023-01-08",
    version = "0.1.4.9001",
    alternative = "command_terminate",
    value = command_delete
  )
  command_terminate <- command_delete %|||% command_terminate
  deprecated <- c(
    "verbose",
    "command_submit",
    "command_terminate",
    "command_delete",
    "script_directory",
    "script_lines"
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
    options_cluster[[arg]] <- value %|||% options_cluster[[arg]]
  }
  launcher <- crew_class_launcher_cluster$new(
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
    crashes_error = crashes_error,
    tls = tls,
    r_arguments = r_arguments,
    options_metrics = options_metrics,
    options_cluster = options_cluster
  )
  launcher$validate()
  launcher
}

#' @title `r lifecycle::badge("maturing")` Abstract cluster launcher class
#' @export
#' @family cluster
#' @keywords internal
#' @description `R6` class to help develop specific cluster launcher plugins.
#' @details See [crew_launcher_cluster()].
#' @inheritSection crew.cluster-package Attribution
crew_class_launcher_cluster <- R6::R6Class(
  classname = "crew_class_launcher_cluster",
  inherit = crew::crew_class_launcher,
  cloneable = FALSE,
  private = list(
    .options_cluster = NULL,
    .prefix = NULL,
    .args_launch = function(script) {
      shQuote(script)
    },
    .args_terminate = function(name) {
      shQuote(name)
    }
  ),
  active = list(
    #' @field options_cluster See [crew_launcher_cluster()].
    options_cluster = function() {
      .subset2(private, ".options_cluster")
    }
  ),
  public = list(
    #' @description Abstract launcher constructor.
    #' @return An abstract launcher object.
    #' @param name See [crew_launcher_cluster()].
    #' @param seconds_interval See [crew_launcher_cluster()].
    #' @param seconds_timeout See [crew_launcher_cluster()].
    #' @param seconds_launch See [crew_launcher_cluster()].
    #' @param seconds_idle See [crew_launcher_cluster()].
    #' @param seconds_wall See [crew_launcher_cluster()].
    #' @param tasks_max See [crew_launcher_cluster()].
    #' @param tasks_timers See [crew_launcher_cluster()].
    #' @param reset_globals See [crew_launcher_cluster()].
    #' @param reset_packages See [crew_launcher_cluster()].
    #' @param reset_options See [crew_launcher_cluster()].
    #' @param garbage_collection See [crew_launcher_cluster()].
    #' @param crashes_error See [crew_launcher_cluster()].
    #' @param tls See [crew_launcher_cluster()].
    #' @param r_arguments See [crew_launcher_cluster()].
    #' @param options_metrics See [crew_launcher_cluster()].
    #' @param options_cluster See [crew_launcher_cluster()].
    initialize = function(
      name = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      crashes_error = NULL,
      tls = NULL,
      r_arguments = NULL,
      options_metrics = NULL,
      options_cluster = NULL
    ) {
      super$initialize(
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
        crashes_error = crashes_error,
        tls = tls,
        r_arguments = r_arguments,
        options_metrics = options_metrics
      )
      private$.options_cluster <- options_cluster
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Throws an error if a field is invalid.
    validate = function() {
      super$validate() # nolint
      crew_options_validate(private$.options_cluster)
      invisible()
    },
    #' @description Launch a local process worker which will
    #'   dial into a socket.
    #' @details The `call` argument is R code that will run to
    #'   initiate the worker.
    #' @return A handle object to allow the termination of the worker
    #'   later on.
    #' @param call Character of length 1, a namespaced call to
    #'   [crew::crew_worker()]
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
      lines <- c(self$script(name = name), paste("Rscript -e", shQuote(call)))
      if (is.null(private$.prefix)) {
        if (!file.exists(private$.options_cluster$script_directory)) {
          dir.create(
            private$.options_cluster$script_directory,
            recursive = TRUE
          )
        }
        private$.prefix <- crew::crew_random_name()
      }
      script <- path_script(
        dir = private$.options_cluster$script_directory,
        prefix = private$.prefix,
        launcher = launcher,
        worker = worker
      )
      writeLines(text = lines, con = script)
      system2(
        command = private$.options_cluster$command_submit,
        args = private$.args_launch(script = script),
        stdout = if_any(private$.options_cluster$verbose, "", FALSE),
        stderr = if_any(private$.options_cluster$verbose, "", FALSE),
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
      if (nzchar(private$.options_cluster$command_terminate)) {
        system2(
          command = private$.options_cluster$command_terminate,
          args = private$.args_terminate(name = handle$name),
          stdout = if_any(private$.options_cluster$verbose, "", FALSE),
          stderr = if_any(private$.options_cluster$verbose, "", FALSE),
          wait = FALSE
        )
      }
      invisible()
    }
  )
)
