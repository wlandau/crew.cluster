library(crew.cluster)
library(testthat)
controller <- crew_controller_sge(
  name = "my_workflow",
  workers = 1L,
  seconds_idle = 300,
  sge_lines = paste0("module load R/", getRversion())
)
controller$start()
controller$push(
  name = "do work",
  command = as.character(Sys.info()["nodename"])
)
controller$wait()
task <- controller$pop()
expect_false(task$result[[1L]] == as.character(Sys.info()["nodename"]))
controller$terminate()
