crew_message <- function(..., frequency = "always", id = NULL) {
  old <- getOption("rlang_backtrace_on_error")
  on.exit(options(rlang_backtrace_on_error = old))
  options(rlang_backtrace_on_error = "none")
  rlang::inform(
    message = paste0(...),
    class = c("crew_message", "crew"),
    .frequency = frequency,
    .frequency_id = id
  )
}
