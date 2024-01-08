#' @title `r lifecycle::badge("experimental")` Create a SGE monitor object.
#' @export
#' @family monitor
#' @description Create an `R6` object to monitor SGE cluster jobs.
#' @inheritParams crew_launcher_cluster
#' @param command_list Character of length 1,
#'   file path to the executable to list jobs.
crew_monitor_sge <- function(
  verbose = TRUE,
  command_list = as.character(Sys.which("qstat")),
  command_terminate = as.character(Sys.which("qdel"))
) {
  out <- crew_class_monitor_sge$new(
    verbose = verbose,
    command_list = command_list,
    command_terminate = command_terminate
  )
  out$validate()
  out
}

#' @title `r lifecycle::badge("experimental")` SGE monitor class
#' @export
#' @family monitor
#' @description SGE monitor `R6` class
#' @details See [crew_monitor_sge()].
crew_class_monitor_sge <- R6::R6Class(
  classname = "crew_class_monitor_sge",
  cloneable = FALSE,
  private = list(
    .verbose = NULL,
    .command_list = NULL,
    .command_terminate = NULL
  ),
  active = list(
    #' @field verbose See [crew_monitor_sge()].
    verbose = function() {
      .subset2(private, ".verbose")
    },
    #' @field command_list See [crew_monitor_sge()].
    command_list = function() {
      .subset2(private, ".command_list")
    },
    #' @field command_terminate See [crew_monitor_sge()].
    command_terminate = function() {
      .subset2(private, ".command_terminate")
    }
  ),
  public = list(
    #' @description SGE monitor constructor.
    #' @return an SGE monitor object.
    #' @param verbose See [crew_monitor_sge()].
    #' @param command_list See [crew_monitor_sge()].
    #' @param command_terminate See [crew_monitor_sge()].
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
    },
    #' @description List SGE jobs.
    #' @return A `tibble` with one row per SGE job and columns with
    #'   specific details.
    #' @param user Character of length 1, user name of the jobs to list.
    jobs = function(user = ps::ps_username()) {
      # Cannot be tested with automated tests.
      # Tested in tests/sge/monitor.R.
      # nocov start
      crew::crew_assert(
        user,
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        message = "'user' must be `NULL` or a character vector of length 1"
      )
      text <- system2(
        "qstat",
        args = shQuote(c("-u", user, "-xml")),
        stdout = TRUE,
        stderr = if_any(private$.verbose, "", FALSE),
        wait = TRUE
      )
      xml <- xml2::read_xml(paste(text, collapse = "\n"))
      jobs <- xml2::as_list(xml2::xml_find_all(xml, "//job_list"))
      job_list <- map(
        jobs,
        ~ tibble::new_tibble(map(.x, ~ unlist(.x) %||% NA), nrow = 1L)
      )
      out <- do.call(vctrs::vec_rbind, job_list)
      names(out) <- tolower(names(out))
      names(out) <- gsub("^jb_|^jat_", "", names(out))
      out
      # nocov end
    },
    #' @description Terminate one or more SGE jobs.
    #' @return `NULL` (invisibly).
    #' @param jobs Character vector of job names or job IDs to terminate.
    #'   Ignored if `all` is set to `TRUE`.
    #' @param all Logical of length 1, whether to terminate all the jobs
    #'   under your user name. This terminates ALL your SGE jobs,
    #'   regardless of whether `crew.cluster` launched them,
    #'   so use with caution!
    terminate = function(jobs = NULL, all = FALSE) {
      # Cannot be tested with automated tests.
      # Tested in tests/sge/monitor.R.
      # nocov start
      crew::crew_assert(
        jobs %||% "x",
        is.character(.),
        !anyNA(.),
        nzchar(.),
        message = paste(
          "'jobs' must be `NULL` or a character vector of",
          "valid job names or IDs."
        )
      )
      crew::crew_assert(
        all,
        isTRUE(.) || isFALSE(.),
        message = "'all' must be TRUE or FALSE."
      )
      args <- shQuote(if_any(all, c("-u", ps::ps_username()), jobs))
      stream <- if_any(private$.verbose, "", FALSE)
      system2(
        command = private$.command_terminate,
        args = args,
        stdout = stream,
        stderr = stream,
        wait = TRUE
      )
      invisible()
      # nocov end
    }
  )
)
