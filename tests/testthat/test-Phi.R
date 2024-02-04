test_that("neural networks don't generate with empty list or lists of size less than 2", {
  0 |>
    Phi() |>
    expect_error()
  0.5 |>
    Phi() |>
    expect_no_error()
  0.1 |>
    Phi() |>
    expect_no_error()

  0 |>
    Phi_k() |>
    expect_error()
  -1 |>
    Phi_k() |>
    expect_error()
  4.5 |>
    Phi_k() |>
    expect_error()
  "Maarufa" |>
    Phi_k() |>
    expect_error()

  1 |>
    Phi_k() |>
    expect_no_error()
  5 |>
    Phi_k() |>
    expect_no_error()
})
