name_job <- function(launcher, worker, instance) {
  out <- paste(launcher, worker, instance, sep = "-")
  alpha <- all(grepl(pattern = "^[[:alpha:]]", x = out))
  if (!alpha) {
    out <- paste0("worker-", out)
  }
  out
}

name_script <- function(prefix, launcher, worker) {
  file.path(sprintf("%s-%s-%s.sh", prefix, launcher, worker))
}
