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
#' @param command_terminate Character of length 1,
#'   file path to the executable to terminate a worker job.
#'   Set to `""` to skip manually terminating the worker.
#'   Unless there is an issue with the platform,
#'   the job should still exit thanks to the NNG-powered network programming
#'   capabilities of `mirai`. Still, if you set `command_terminate = ""`,
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
#' @examples
#'   crew_options_cluster()
crew_options_cluster <- function(
  verbose = FALSE,
  command_submit = as.character(Sys.which("qsub")),
  command_terminate = as.character(Sys.which("qdel")),
  script_directory = tempdir(),
  script_lines = character(0L)
) {
  out <- structure(
    list(
      verbose = verbose,
      command_submit = command_submit,
      command_terminate = command_terminate,
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
  fields <- c("command_submit", "command_submit", "script_directory")
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
