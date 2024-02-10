test_that("neural networks don't generate with empty list or lists of size less than 2", {
  expect_error(create_nn(c()))
  expect_error(create_nn(c(5)))
})
