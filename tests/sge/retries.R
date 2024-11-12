# Watch the number of cores allocated to each worker instance


library(crew.cluster)
library(testthat)

test_that("SGE retries", {
  x <- crew_controller_sge(
    name = "123",
    workers = 1L,
    seconds_launch = 604800,
    seconds_idle = 300,
    options_cluster = crew_options_sge(
      verbose = TRUE,
      script_lines = paste0("module load R/", getRversion()),
      cores = c(1, 2, 3)
    ),
    crashes_error = 4L
  )
  on.exit(x$terminate())
  x$start()
  x$push(Sys.sleep(1e3), scale = TRUE) # 1 slot
  Sys.sleep(2)
  x$scale()
  expect_equal(x$launcher$workers$crashes[[1L]], 0L)
  Sys.sleep(5)
  x$launcher$terminate_worker(handle = x$launcher$workers$handle[[1L]])
  Sys.sleep(5)
  x$scale() # 2 slots
  expect_equal(x$launcher$workers$crashes[[1L]], 1L)
  Sys.sleep(5)
  x$launcher$terminate_worker(handle = x$launcher$workers$handle[[1L]])
  Sys.sleep(5)
  x$scale() # 3 slots
  expect_equal(x$launcher$workers$crashes[[1L]], 2L)
  Sys.sleep(5)
  x$launcher$terminate_worker(handle = x$launcher$workers$handle[[1L]])
  Sys.sleep(5)
  x$scale() # 3 slots again
  expect_equal(x$launcher$workers$crashes[[1L]], 3L)
  Sys.sleep(5)
  x$launcher$terminate_worker(handle = x$launcher$workers$handle[[1L]])
  Sys.sleep(5)
  expect_error(x$scale(), class = "crew_error")
})
