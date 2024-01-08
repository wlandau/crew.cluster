test_that("LSF minimal", {
  controller <- crew_controller_lsf(
    name = "my_workflow",
    workers = 1L,
    seconds_idle = 300,
    script_lines = paste0("module load R/", getRversion()),
    verbose = TRUE
  )
  on.exit(controller$terminate())
  controller$start()
  controller$push(
    name = "do work",
    command = as.character(Sys.info()["nodename"])
  )
  controller$wait()
  task <- controller$pop()
  expect_false(task$result[[1L]] == as.character(Sys.info()["nodename"]))
})
