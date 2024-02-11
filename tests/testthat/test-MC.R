test_that("The MC network works as expected", {
  seq(0, 5, length.out = 500) |> matrix(1, 500) -> X
  sin(X) |> matrix(500, 1) -> y
  1 -> L
  X |>
    MC(y, 1) |>
    inst(ReLU, 3) |>
    expect_no_error()
  X |>
    MC(y, 1) |>
    inst(Sigmoid, 3) |>
    expect_no_error()
  X |>
    MC(y, 1) |>
    inst(Tanh, 3) |>
    expect_no_error()
  5 |>
    MC(y,1) |>
    expect_error()
})
