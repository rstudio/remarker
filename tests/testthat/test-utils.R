test_that("interleave()", {
  expect_identical(interleave(1:3, 4:6), as.integer(c(1, 4, 2, 5, 3, 6)))
  expect_identical(interleave(1:3, 4:5), as.integer(c(1, 4, 2, 5, 3)))
  expect_identical(interleave(integer(0), integer(0)), integer(0))
  expect_identical(interleave(1L, integer(0)), 1L)
  expect_error(interleave(1:3, 4))
  expect_error(interleave(1:3, 4:7))
})
