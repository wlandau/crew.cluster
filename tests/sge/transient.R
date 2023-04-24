library(crew.cluster)
library(testthat)
x <- crew_controller_sge(
  name = "my_workflow",
  tasks_max = 1L,
  workers = 4L,
  sge_lines = paste0("module load R/", getRversion())
)
x$start()
n <- 200
for (index in seq_len(n)) {
  name <- paste0("task_", index)
  x$push(name = name, command = as.character(Sys.info()["nodename"]))
  message(paste("push", name))
}
x$wait(mode = "all")
expect_equal(length(x$queue), 0L)
expect_equal(length(x$results), 200L)
results <- list()
while (length(results) < n) {
  out <- x$pop()
  if (!is.null(out)) {
    results[[length(results) + 1L]] <- out
    message(paste("done", out$name, out$result[[1]]))
  }
}
expect_equal(length(results), n)
results <- tibble::as_tibble(do.call(rbind, results))
results$result <- as.character(results$result)
expect_false(anyNA(results$result))
expect_false(any(results$result == as.character(Sys.info()["nodename"])))
expect_equal(results$error, rep(NA_character_, n))
expect_equal(
  sort(paste0("task_", seq_len(n))),
  sort(unique(results$name))
)
out <- x$summary()
expect_equal(sum(out$worker_launches), n)
x$terminate()
