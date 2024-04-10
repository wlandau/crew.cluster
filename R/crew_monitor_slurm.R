#' @title `r lifecycle::badge("experimental")` Create a SLURM monitor object.
#' @export
#' @family slurm
#' @description Create an `R6` object to monitor SLURM cluster jobs.
#' @inheritParams crew_monitor_cluster
crew_monitor_slurm <- function(
  verbose = TRUE,
  command_list = as.character(Sys.which("squeue")),
  command_terminate = as.character(Sys.which("scancel"))
) {
  out <- crew_class_monitor_slurm$new(
    verbose = verbose,
    command_list = command_list,
    command_terminate = command_terminate
  )
  out$validate()
  out
}

#' @title `r lifecycle::badge("experimental")` SLURM monitor class
#' @export
#' @family slurm
#' @description SLURM monitor `R6` class
#' @details See [crew_monitor_slurm()].
crew_class_monitor_slurm <- R6::R6Class(
  classname = "crew_class_monitor_slurm",
  inherit = crew_class_monitor_cluster,
  cloneable = FALSE,
  public = list(
    #' @description List SLURM jobs.
    #' @details This function loads the entire SLURM queue for all users,
    #'   so it may take several seconds to execute.
    #'   It is intended for interactive use, and
    #'   should especially be avoided in scripts where it is called
    #'   frequently. It requires SLURM version 20.02 or higher,
    #'   along with the YAML plugin.
    #' @return A `tibble` with one row per SLURM job and columns with
    #'   specific details.
    #' @param user Character of length 1, user name of the jobs to list.
    jobs = function(user = ps::ps_username()) {
      # Cannot be tested with automated tests.
      # Tested in tests/slurm/monitor.R.
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
        private$.command_list,
        args = shQuote(c("--yaml")),
        stdout = TRUE,
        stderr = if_any(private$.verbose, "", FALSE),
        wait = TRUE
      )
      monitor_cols <- c("job_id", "partition", "name", "user_name", "job_state",
                        "start_time", "node_count", "state_reason")
      yaml <- yaml::read_yaml(text = text)
      out <- map(
        yaml$jobs,
        ~ tibble::new_tibble(
          c(
            map(.x[monitor_cols], ~ unlist(.x) %||% NA),
            list(
              nodes = paste(
                unlist(.x$job_resources$nodes),
                collapse = ","
              ) %||% NA
            )
          )
        )
      )
      out <- do.call(vctrs::vec_rbind, out)
      out <- out[out$user_name == user, ]
      out <- out[which(out$job_state != "CANCELLED"), ]
      out$job_id <- as.character(out$job_id)
      out$start_time <- as.POSIXct(out$start_time, origin = "1970-01-01")
      out
      # nocov end
    },
    #' @description Terminate one or more SLURM jobs.
    #' @return `NULL` (invisibly).
    #' @param jobs Character vector of job names or job IDs to terminate.
    #'   Ignored if `all` is set to `TRUE`.
    #' @param all Logical of length 1, whether to terminate all the jobs
    #'   under your user name. This terminates ALL your SLURM jobs,
    #'   regardless of whether `crew.cluster` launched them,
    #'   so use with caution!
    terminate = function(jobs = NULL, all = FALSE) {
      # Cannot be tested with automated tests.
      # Tested in tests/slurm/monitor.R.
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
