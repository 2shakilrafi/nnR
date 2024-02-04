test_that("neural networks don't generate with empty list or lists of size less than 2", {
  expect_error(create_neural_network(c()))
  expect_error(create_neural_network(c(5)))
})
