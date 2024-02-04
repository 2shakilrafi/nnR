test_that("neural networks don't generate with empty list or lists of size less than 2", {
  8 |>
    Trp() |>
    expect_no_error()
  -4 |>
    Trp() |>
    expect_error()

  Etr(4, 0.1) |> expect_no_error()
  Etr(0, 0.1) |> expect_error()
  Etr(4, 0) |> expect_error()
})
