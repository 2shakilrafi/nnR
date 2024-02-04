test_that("That Prd appropriately checks for q and eps", {
  Pwr(1.5, 0.1, 2) |> expect_error()
  Pwr(2.1, -0.5, 2) |> expect_error()

  Pwr(2.1, 0.1, 0) |> expect_no_error()
  Pwr(2.1, 0.1, 1) |> expect_no_error()
  Pwr(2.1, 0.1, 2) |> expect_no_error()
  Pwr(2.1, 0.1, 3) |> expect_no_error()
  Pwr(2.1, 0.1, 4) |> expect_no_error()
})
