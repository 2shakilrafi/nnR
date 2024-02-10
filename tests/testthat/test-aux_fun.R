test_that("Auxilliary functions work", {
  create_nn(c(4, 5, 6, 234)) |>
    inn() |>
    expect_no_error()
  create_nn(c(4, 5, 6, 234)) |>
    out() |>
    expect_no_error()
  create_nn(c(4, 5, 6, 234)) |>
    param() |>
    expect_no_error()
  create_nn(c(4, 5, 6, 234)) |>
    dep() |>
    expect_no_error()
  create_nn(c(4, 5, 6, 234)) |>
    hid() |>
    expect_no_error()
  create_nn(c(4, 5, 6, 234)) |>
    lay() |>
    expect_no_error()

  5 |>
    inn() |>
    expect_error()
  5 |>
    out() |>
    expect_error()
  5 |>
    param() |>
    expect_error()
  5 |>
    dep() |>
    expect_error()
  5 |>
    hid() |>
    expect_error()
  5 |>
    lay() |>
    expect_error()
})
