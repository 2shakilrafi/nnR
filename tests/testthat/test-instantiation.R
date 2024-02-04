test_that("Only neural networks can be instantiated", {
  inst("Maarufa", ReLU, 5) |> expect_error()
})

test_that("x and input to neural network must match", {
  Prd(2.1, 0.1) |>
    inst(ReLU, 5) |>
    expect_error()
  Prd(2.1, 0.1) |>
    inst(ReLU, c(5, 5)) |>
    expect_no_error()

  Prd(2.1, 0.1) |>
    inst(Sigmoid, c(5, 5)) |>
    expect_no_error()
  Prd(2.1, 0.1) |>
    inst(Tanh, c(5, 5)) |>
    expect_no_error()
})
