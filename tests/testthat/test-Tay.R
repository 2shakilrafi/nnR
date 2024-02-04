test_that("neural networks don't generate with empty list or lists of size less than 2", {
  Tay("exp", 3, 2.1, 0.1) |> expect_no_error()
  Tay("cos", 3, 2.1, 0.1) |> expect_no_error()
  Tay("sin", 3, 2.1, 0.1) |> expect_no_error()
})
