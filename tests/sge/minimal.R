test_that("SGE minimal", {
  controller <- crew_controller_sge(
    name = "my_workflow",
    workers = 1L,
    seconds_launch = 604800,
    seconds_idle = 300,
    script_lines = paste0("module load R/", getRversion()),
    verbose = TRUE
  )
  on.exit(controller$terminate())
  controller$start()
  controller$push( # Should see a job submission message.
    name = "do work",
    command = as.character(Sys.info()["nodename"])
  )
  controller$wait()
  task <- controller$pop()
  expect_false(task$result[[1L]] == as.character(Sys.info()["nodename"]))
  controller$launcher$terminate() # Should see a job deletion message.
  Sys.sleep(5L)
})
