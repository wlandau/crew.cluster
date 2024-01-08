map <- function(x, f, ...) {
  lapply(X = x, FUN = as_function(f), ...)
}
