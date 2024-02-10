test_that("neural networks don't generate with empty list or lists of size less than 2", {
  create_nn(c(4, 5, 6)) |>
    stk(create_nn(c(5, 5, 5))) |>
    expect_no_error()
  5 |>
    stk(6) |>
    expect_error()

  create_nn(c(4, 5, 6)) |>
    stk(create_nn(c(5, 5, 5, 5, 5, 5))) |>
    expect_no_error()
  create_nn(c(5, 5, 5, 5, 5, 5)) |>
    stk(create_nn(c(4, 5, 6))) |>
    expect_no_error()

  create_nn(c(4, 5, 6)) %stk% create_nn(c(5, 5, 5, 5, 5, 5)) |> expect_no_error()
  create_nn(c(5, 5, 5, 5, 5, 5)) %stk% create_nn(c(4, 5, 6)) |> expect_no_error()
})
