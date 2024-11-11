test_that("%||%", {
  expect_equal("a" %||% "b", "a")
  expect_equal(list() %||% "b", "b")
  expect_equal(NULL %||% "b", "b")
})

test_that("%|||%", {
  expect_equal("a" %|||% "b", "a")
  expect_equal(list() %|||% "b", list())
  expect_equal(NULL %|||% "b", "b")
})

test_that("if_any()", {
  expect_equal(if_any(TRUE, "a", "b"), "a")
  expect_equal(if_any(FALSE, "a", "b"), "b")
})

test_that("slice_bounded", {
  for (index in seq_len(3L)) {
    expect_equal(slice_bounded(NULL, index), NULL)
    expect_equal(slice_bounded(character(0L), index), character(0L))
    expect_equal(slice_bounded(integer(0L), index), integer(0L))
  }
  x <- c("a", "b", "c")
  expect_equal(slice_bounded(x, 1L), "a")
  expect_equal(slice_bounded(x, 2L), "b")
  for (index in seq(3L, 10L)) {
    expect_equal(slice_bounded(x, index), "c")
  }
})
