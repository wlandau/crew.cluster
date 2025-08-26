test_that("PBS minimal", {
  controller <- crew_controller_pbs(
    name = "my_workflow",
    workers = 1L,
    seconds_idle = 300,
    options_cluster = crew_options_pbs(
      script_lines = paste0("module load R/", getRversion()),
      verbose = TRUE
    )
  )
  on.exit(controller$terminate())
  controller$start()
  controller$push(
    # Should see a job submission message.
    name = "do work",
    command = as.character(Sys.info()["nodename"])
  )
  controller$wait()
  task <- controller$pop()
  expect_false(task$result[[1L]] == as.character(Sys.info()["nodename"]))
  controller$launcher$terminate()
  Sys.sleep(5L)
})
