test_that("SLURM minimal", {
  controller <- crew_controller_slurm(
    name = "my_workflow",
    workers = 1L,
    seconds_idle = 300,
    options_cluster = crew_options_slurm(
      script_lines = paste0("module load R/", getRversion()),
      verbose = TRUE
    )
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
