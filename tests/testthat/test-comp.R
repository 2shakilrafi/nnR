test_that("Only neural networks may be composed", {
  expect_error(comp(c(), create_neural_network(c(5, 6, 7))))
  expect_error(comp(5, create_neural_network(c(5, 6, 7))))

  create_neural_network(c(5, 6, 7)) |>
    comp(create_neural_network(c(5, 6, 7))) |>
    expect_error()
  create_neural_network(c(5, 6, 7)) %comp% create_neural_network(c(5, 6, 7)) |> expect_error()
  create_neural_network(c(7, 6, 5)) %comp% create_neural_network(c(1, 6, 7)) |>
    inst(ReLU, 4) |>
    expect_no_error()

  create_neural_network(c(5, 6, 7)) %comp% create_neural_network(c(5, 6, 7)) |> expect_error()
  create_neural_network(c(7, 6, 5)) |>
    comp(create_neural_network(c(1, 6, 7))) |>
    inst(ReLU, 4) |>
    expect_no_error()

  c() %comp% Tun(5) |> expect_error()
  5 %comp% Tun(5) |> expect_error()
})
