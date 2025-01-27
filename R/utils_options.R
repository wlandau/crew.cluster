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
