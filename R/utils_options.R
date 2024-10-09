crew_options_validate <- function(options) {
  UseMethod("crew_options_validate")
}

#' @export
crew_options_validate.default <- function(options) {
  crew::crew_assert(
    inherits(options, c("crew_options_cluster", "crew_options")),
    message = paste(
      "options object should come from",
      "a crew_options_*() function in {crew.cluster}."
    )
  )
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
