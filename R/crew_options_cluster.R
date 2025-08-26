#' @title Common abstract cluster options.
#' @export
#' @family cluster
#' @keywords internal
#' @description Common abstract cluster options.
#' @inheritSection crew.cluster-package Attribution
#' @return A classed list of options.
#' @param verbose Logical, whether to see console output and error messages
#'   when submitting worker.
#' @param command_submit Character of length 1,
#'   file path to the executable to submit a worker job.
#' @param command_terminate Deprecated on 2025-08-26 in
#'   `crew.cluster` version 0.3.8.9001. No longer needed.
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
#' @examples
#'   crew_options_cluster()
crew_options_cluster <- function(
  verbose = FALSE,
  command_submit = as.character(Sys.which("qsub")),
  command_terminate = NULL,
  script_directory = tempdir(),
  script_lines = character(0L)
) {
  crew::crew_deprecate(
    name = "command_terminate",
    date = "2025-08-26",
    version = "0.3.8.9001",
    alternative = paste(
      "none (no longer needed,
      c.f. https://github.com/wlandau/crew/issues/236)."
    ),
    condition = "message",
    value = command_terminate
  )
  out <- structure(
    list(
      verbose = verbose,
      command_submit = command_submit,
      script_directory = script_directory,
      script_lines = script_lines
    ),
    class = c("crew_options_cluster", "crew_options")
  )
  crew_options_validate(out)
  out
}

#' @export
crew_options_validate.crew_options_cluster <- function(options) {
  crew::crew_assert(
    options$verbose,
    isTRUE(.) || isFALSE(.),
    message = "the \"verbose\" field is not a length-1 logical."
  )
  fields <- c("command_submit", "script_directory")
  for (field in fields) {
    crew::crew_assert(
      options[[field]],
      is.character(.),
      length(.) == 1L,
      !anyNA(.),
      message = paste(field, "must be a valid length-1 character string.")
    )
  }
  if (!is.null(options$script_lines)) {
    crew::crew_assert(
      options$script_lines,
      is.character(.),
      !anyNA(.),
      message = "invalid script_lines field"
    )
  }
}
