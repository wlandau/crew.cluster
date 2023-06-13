path_script <- function(dir, prefix, launcher, worker) {
  file.path(dir, sprintf("%s-%s-%s.sh", prefix, launcher, worker))
}
