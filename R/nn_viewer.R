source("R/is_nn.R")
source("R/aux_fun.R")


#' @title view_nn
#'
#' @description Takes a neural network shown in vectorized form and explicitly
#' displays it.
#'
#' @param nn A neural network., i.e.
#' a list of lists of \eqn{W} and \eqn{b}.
#'
#' @examples
#' c(5, 6, 7, 9) |>
#'   create_nn() |>
#'   view_nn()
#' Sqr(2.1, 0.1) |> view_nn()
#'
#' @return A displayed version of the neural network. This may be required if
#' the neural network is very deep.
#' @export
#'
#' @examples
#' Xpn(3, 2.1, 1.1) |> view_nn()
#' Pwr(2.1, 0.1, 3) |> view_nn()
view_nn <- function(nn) {
  if (nn |> is_nn() == TRUE) {
    for (i in 1:dep(nn)) {
      print(nn[i])
    }
  } else {
    stop("You may only view neural networks")
  }
}
