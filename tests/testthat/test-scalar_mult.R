test_that("a must be a number and nu a neural network and they must be in the
          right place", {
  c(5, 6) |>
    slm(c(4, 5)) |>
    expect_error()
  c(5, 6) |>
    srm(c(4, 5)) |>
    expect_error()
  5 |>
    srm(create_nn(c(4, 3, 2))) |>
    expect_error()
  create_nn(c(4, 3, 1)) |>
    slm(5) |>
    expect_error()

  c(5, 6) %slm% c(4, 5) |> expect_error()
  c(5, 6) %srm% c(4, 5) |> expect_error()
  5 %srm% create_nn(c(4, 3, 2)) |> expect_error()
  create_nn(c(4, 3, 1)) %slm% 5 |> expect_error()

  5 |>
    slm(Tun(5)) |>
    expect_no_error()
  6 |>
    slm(Tun(5, d = 7)) |>
    expect_no_error()

  Tun(5) |>
    srm(4) |>
    expect_no_error()
  Tun(5, d = 7) |>
    srm(6) |>
    expect_no_error()

  5 %slm% Tun(5) |> expect_no_error()
  6 %slm% Tun(5, d = 7) |> expect_no_error()

  Tun(5) %srm% 4 |> expect_no_error()
  Tun(5, d = 7) %srm% 6 |> expect_no_error()
})
