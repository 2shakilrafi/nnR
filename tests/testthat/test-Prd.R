test_that("That Prd appropriately checks for q and eps", {
  Prd(0, -3) |> expect_error()
  Prd(2, 0) |> expect_error()
})
