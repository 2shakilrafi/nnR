test_that("Activation functions only take real numbers", {
  expect_error(ReLU(c()))
  expect_error(ReLU(c(5, 5)))

  c() |>
    Sigmoid() |>
    expect_error()
  c(5, 5) |>
    Sigmoid() |>
    expect_error()

  c() |>
    Tanh() |>
    expect_error()
  c(5, 5) |>
    Tanh() |>
    expect_error()
})
