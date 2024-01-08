test_that("map()", {
  expect_equal(unname(map(letters, identity)), as.list(letters))
})
