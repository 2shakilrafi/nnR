source("R/Prd.R")
source("R/Aff.R")
source("R/stacking.R")
source("R/Tun.R")
source("R/aux_fun.R")

#' @title Pwr
#' @description
#' A function that returns the \eqn{\mathsf{Pwr}} neural networks.
#'
#'
#' @param q inside \eqn{(2,\infty)}.
#' @param eps inside \eqn{(0,\infty)}.
#' @param exponent the exponent which the Pwr network will approximate. Must be
#' a non-negative integer.
#'
#' @return A neural network that approximates raising a number to exponent, when
#' given appropriate \eqn{q,\varepsilon} and exponent when isntanatiated
#' under ReLU activation at \eqn{x}.
#' @export


Pwr <- function(q, eps, exponent) {
  if (q <= 2) {
    stop("Too small q, q must be >= 2")
  } else if (eps <= 0) {
    stop("Too small eps, eps must be >= 0")
  } else if (exponent %% 1 != 0 || exponent < 0) {
    stop("Exponent must be a non-negative integer")
  } else {
    if (exponent == 0) {
      Aff(0, 1) -> return_network
      return(return_network)
    } else if (exponent >= 1) {
      Cpy(2, 1) -> first_third
      Pwr(q, eps, exponent - 1) |> stk(Pwr(q, eps, exponent - 1) |> dep() |> Tun()) -> mid_third
      Prd(q, eps) -> last_third
      last_third |>
        comp(mid_third) |>
        comp(first_third) -> return_network
    } else {
      return("Invalid exponent, must be non-negative integer")
    }
    return(return_network)
  }
}
