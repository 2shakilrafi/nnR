test_that("Activation functions only take real numbers or lists of real numbers", {
  expect_no_error(ReLU(c()))
  expect_no_error(ReLU(c(5, 5)))

  c() |>
    Sigmoid() |>
    expect_no_error()
  c(5, 5) |>
    Sigmoid() |>
    expect_no_error()

  c() |>
    Tanh() |>
    expect_no_error()
  c(5, 5) |>
    Tanh() |>
    expect_no_error()

  "Maarufa" |>
    Tanh() |> expect_error()
})
