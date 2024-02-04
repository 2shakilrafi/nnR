test_that("neural networks don't generate with empty list or lists of size less than 2", {
  Xpn(3, 2.1, 0.1) |> expect_no_error()
  Csn(3, 2.1, 0.1) |> expect_no_error()
  Sne(3, 2.1, 0.1) |> expect_no_error()

  Xpn(-1, 2.2, 0.1) |> expect_error()
  Xpn(2, 1.5, 0.1) |> expect_error()
  Xpn(2, 2.1, 0) |> expect_error()

  Csn(-1, 2.2, 0.1) |> expect_error()
  Csn(2, 1.5, 0.1) |> expect_error()
  Csn(2, 2.1, 0) |> expect_error()

  Sne(-1, 2.2, 0.1) |> expect_error()
  Sne(2, 1.5, 0.1) |> expect_error()
  Sne(2, 2.1, 0) |> expect_error()
})
