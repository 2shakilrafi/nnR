test_that("neural networks don't generate with empty list or lists of size less than 2", {
  seq(0, 5, length.out = 500) |> matrix(1, 500) -> X
  sin(X) |> matrix(500, 1) -> y
  1 -> L
  X |>
    P(y, 1) |>
    inst(ReLU, 3) |>
    expect_no_error()
  X |>
    P(y, 1) |>
    inst(Sigmoid, 3) |>
    expect_no_error()
  X |>
    P(y, 1) |>
    inst(Tanh, 3) |>
    expect_no_error()
})
