test_that("The MC network works as expected", {
  5 |> view_nn() |> expect_error()

  create_nn(c(4,5,4,5)) |> view_nn() |> expect_no_error()
})
