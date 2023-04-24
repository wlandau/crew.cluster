name_job <- function(launcher, worker, instance) {
  out <- paste(launcher, worker, instance, sep = "-")
  alpha <- all(grepl(pattern = "^[[:alpha:]]", x = out))
  if (!alpha) {
    out <- paste0("sge-", out)
  }
  out
}
