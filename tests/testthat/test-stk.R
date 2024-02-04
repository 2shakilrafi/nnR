test_that("neural networks don't generate with empty list or lists of size less than 2", {
  create_neural_network(c(4, 5, 6)) |>
    stk(create_neural_network(c(5, 5, 5))) |>
    expect_no_error()
  5 |>
    stk(6) |>
    expect_error()

  create_neural_network(c(4, 5, 6)) |>
    stk(create_neural_network(c(5, 5, 5, 5, 5, 5))) |>
    expect_no_error()
  create_neural_network(c(5, 5, 5, 5, 5, 5)) |>
    stk(create_neural_network(c(4, 5, 6))) |>
    expect_no_error()

  create_neural_network(c(4, 5, 6)) %stk% create_neural_network(c(5, 5, 5, 5, 5, 5)) |> expect_no_error()
  create_neural_network(c(5, 5, 5, 5, 5, 5)) %stk% create_neural_network(c(4, 5, 6)) |> expect_no_error()
})
