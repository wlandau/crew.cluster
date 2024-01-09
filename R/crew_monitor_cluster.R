#' @title `r lifecycle::badge("experimental")` Create an abstract
#'   cluster monitor object.
#' @export
#' @family cluster
#' @keywords internal
#' @description Create an abstract cluster monitor `R6` object.
#' @inheritParams crew_launcher_cluster
#' @param command_list Character of length 1,
#'   file path to the executable to list jobs.
crew_monitor_cluster <- function(
  verbose = TRUE,
  command_list = as.character(Sys.which("qstat")),
  command_terminate = as.character(Sys.which("qdel"))
) {
  out <- crew_class_monitor_cluster$new(
    verbose = verbose,
    command_list = command_list,
    command_terminate = command_terminate
  )
  out$validate()
  out
}

#' @title `r lifecycle::badge("experimental")` Abstract cluster monitor class
#' @export
#' @family cluster
#' @keywords internal
#' @description Abstract cluster monitor `R6` class
#' @details See [crew_monitor_cluster()].
crew_class_monitor_cluster <- R6::R6Class(
  classname = "crew_class_monitor_cluster",
  cloneable = FALSE,
  private = list(
    .verbose = NULL,
    .command_list = NULL,
    .command_terminate = NULL
  ),
  active = list(
    #' @field verbose See [crew_monitor_cluster()].
    verbose = function() {
      .subset2(private, ".verbose")
    },
    #' @field command_list See [crew_monitor_cluster()].
    command_list = function() {
      .subset2(private, ".command_list")
    },
    #' @field command_terminate See [crew_monitor_cluster()].
    command_terminate = function() {
      .subset2(private, ".command_terminate")
    }
  ),
  public = list(
    #' @description Abstract cluster monitor constructor.
    #' @return an abstract cluster monitor object.
    #' @param verbose See [crew_monitor_cluster()].
    #' @param command_list See [crew_monitor_cluster()].
    #' @param command_terminate See [crew_monitor_cluster()].
    initialize = function(
      verbose = NULL,
      command_list = NULL,
      command_terminate = NULL
    ) {
      private$.verbose <- verbose
      private$.command_list <- command_list
      private$.command_terminate <- command_terminate
    },
    #' @description Validate the monitor.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew::crew_assert(
        private$.verbose,
        isTRUE(.) || isFALSE(.),
        message = "'verbose' must be TRUE or FALSE."
      )
      for (field in c(".command_terminate", ".command_list")) {
        crew::crew_assert(
          private[[field]],
          is.character(.),
          length(.) == 1L,
          !anyNA(.),
          message = paste(field, "must be a valid length-1 character string.")
        )
      }
      invisible()
    }
  )
)
