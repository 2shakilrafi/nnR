test_that("Only neural networks may be composed", {
  expect_error(comp(c(), create_nn(c(5, 6, 7))))
  expect_error(comp(5, create_nn(c(5, 6, 7))))

  create_nn(c(5, 6, 7)) |>
    comp(create_nn(c(5, 6, 7))) |>
    expect_error()
  create_nn(c(5, 6, 7)) %comp% create_nn(c(5, 6, 7)) |> expect_error()
  create_nn(c(7, 6, 5)) %comp% create_nn(c(1, 6, 7)) |>
    inst(ReLU, 4) |>
    expect_no_error()

  create_nn(c(5, 6, 7)) %comp% create_nn(c(5, 6, 7)) |> expect_error()
  create_nn(c(7, 6, 5)) |>
    comp(create_nn(c(1, 6, 7))) |>
    inst(ReLU, 4) |>
    expect_no_error()

  c() %comp% Tun(5) |> expect_error()
  5 %comp% Tun(5) |> expect_error()
})
