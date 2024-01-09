test_that("SGE monitor terminate one job at a time", {
  controller <- crew_controller_sge(
    name = "my_workflow",
    workers = 2L,
    seconds_idle = 600,
    script_lines = paste0("module load R/", getRversion()),
    verbose = TRUE
  )
  on.exit(controller$terminate())
  controller$start()
  controller$launch(n = 2L)
  names <- vapply(
    controller$launcher$workers$handle,
    function(handle) handle$name,
    FUN.VALUE = character(1L)
  )
  monitor <- crew_monitor_sge(verbose = TRUE)
  crew::crew_retry(
    ~ all(names %in% monitor$jobs()$name),
    seconds_interval = 1,
    seconds_timeout = 60,
    message = "could not submit jobs"
  )
  jobs <- monitor$jobs()
  jobs <- jobs[jobs$name %in% names, ]
  expect_equal(nrow(jobs), 2L)
  expect_true(is.character(jobs$job_number))
  expect_false(anyNA(jobs$job_number))
  expect_true(all(nzchar(jobs$job_number)))
  monitor$terminate(jobs = jobs$job_number)
  crew::crew_retry(
    ~ !any(jobs$job_number %in% monitor$jobs()$job_number),
    seconds_interval = 1,
    seconds_timeout = 60,
    message = "could not terminate jobs"
  )
  expect_false(any(jobs$job_number %in% monitor$jobs()$job_number))
})

test_that("THIS TEST DELETES ALL USER JOBS! USE WITH CAUTION!", {
  controller <- crew_controller_sge(
    name = "my_workflow",
    workers = 2L,
    seconds_idle = 600,
    script_lines = paste0("module load R/", getRversion()),
    verbose = TRUE
  )
  on.exit(controller$terminate())
  controller$start()
  controller$launch(n = 2L)
  names <- vapply(
    controller$launcher$workers$handle,
    function(handle) handle$name,
    FUN.VALUE = character(1L)
  )
  monitor <- crew_monitor_sge(verbose = TRUE)
  crew::crew_retry(
    ~ all(names %in% monitor$jobs()$name),
    seconds_interval = 1,
    seconds_timeout = 60,
    message = "could not submit jobs"
  )
  jobs <- monitor$jobs()
  jobs <- jobs[jobs$name %in% names, ]
  expect_equal(nrow(jobs), 2L)
  expect_true(is.character(jobs$job_number))
  expect_false(anyNA(jobs$job_number))
  expect_true(all(nzchar(jobs$job_number)))
  monitor$terminate(all = TRUE)
  crew::crew_retry(
    ~ nrow(monitor$jobs()) < 1L,
    seconds_interval = 1,
    seconds_timeout = 60,
    message = "could not terminate jobs"
  )
  expect_lt(nrow(monitor$jobs()), 1L)
})
