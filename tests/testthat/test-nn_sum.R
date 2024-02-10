test_that("Only neural networks may be summed", {
  5 |>
    nn_sum(6) |>
    expect_error()
  c(5, 6) |>
    nn_sum(6) |>
    expect_error()
  6 |>
    nn_sum(c(5, 6)) |>
    expect_error()
  6 %nn_sum% c(5, 6) |> expect_error()
  c(5, 6) %nn_sum% 6 |> expect_error()

  create_nn(c(4, 4, 4, 4)) %nn_sum% create_nn(c(9, 9, 10)) |>
    expect_error()

  create_nn(c(4, 4, 4, 4)) |>
    nn_sum(create_nn(c(9, 9, 10))) |>
    expect_error()

  create_nn(c(4, 4, 4, 4, 4)) %nn_sum% create_nn(c(4, 4, 5, 4)) |> expect_no_error()
})
