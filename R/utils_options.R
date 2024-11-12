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

crew_options_slice <- function(options, index) {
  exclude <- names(formals(crew_options_cluster))
  for (name in setdiff(names(options), exclude)) {
    options[[name]] <- slice_bounded(options[[name]], index)
  }
  options
}

slice_bounded <- function(x, index) {
  x[min(index, length(x))]
}
