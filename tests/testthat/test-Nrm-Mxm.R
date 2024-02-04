test_that("Activation functions only take real numbers", {
  Mxm(0) |> expect_error()
  Nrm(0) |> expect_error()

  Mxm(1) |> expect_no_error()
  Mxm(2) |> expect_no_error()
  Mxm(3) |> expect_no_error()
  Mxm(4) |> expect_no_error()
  Mxm(5) |> expect_no_error()

  Nrm(1) |> expect_no_error()
  Nrm(2) |> expect_no_error()
  Nrm(3) |> expect_no_error()
})
