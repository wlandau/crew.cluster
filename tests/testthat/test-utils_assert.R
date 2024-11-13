test_that("crew_message()", {
  expect_message(crew_message("a", "b"), class = "crew_message")
})
