name_job <- function(launcher, worker, instance) {
  out <- paste(launcher, worker, instance, sep = "-")
  alpha <- all(grepl(pattern = "^[[:alpha:]]", x = out))
  if (!alpha) {
    out <- paste0("worker-", out)
  }
  out
}

path_script <- function(dir, prefix, launcher, worker) {
  file.path(dir, sprintf("%s-%s-%s.sh", prefix, launcher, worker))
}
