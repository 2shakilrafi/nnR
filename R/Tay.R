source("R/Pwr.R")
source("R/nn_sum.R")
source("R/scalar_mult.R")
source("R/Aff.R")

#' The Tay function
#'
#' @param f the function to be Taylor approximated, for now "exp", "sin"
#' and "cos". NOTE use the quotation marks when using this argument.
#' @param n The number of Taylor iterations. Accuracy as well as computation
#' time increases as \eqn{n} increases
#' @param q a real number in \eqn{(2,\infty)}. Accuracy as well as computation
#' time increases as \eqn{q} gets closer to \eqn{2} increases
#' @param eps a real number in \eqn{(0,\infty)}. ccuracy as well as computation
#' time increases as \eqn{\varepsilon} gets closer to \eqn{0} increases
#'
#' @examples
#' Tay("sin", 2, 2.3, 0.3) |> inst(ReLU, 1.5) # May take some time, please only click once
#' Tay("cos", 2, 2.3, 0.3) |> inst(ReLU, 1) # May take some time, please only click once
#' Tay("exp", 4, 2.3, 0.3) |> inst(ReLU, 1.5) # May take some time, please only click once
#'
#'
#' @return a neural network that approximates the function f. For now only
#' \eqn{sin}, \eqn{cos}, and \eqn{e^x} are available.

Tay <- function(f, n, q, eps) {
  if (n %% 1 != 0 || n < 0) {
    stop("Number of Taylor iteration must be a non negative integer")
  } else if (q < 2 || eps < 0) {
    stop("q must be > 2 and eps must be > 0")
  } else if (f != "exp" && f != "sin" && f != "cos") {
    stop("For now, only Taylor approximations for exp, sin, and cos is available")
  } else {
    if (f == "exp") {
      (1 / factorial(0)) |> slm(Pwr(q, eps, 0)) -> return_network
      if (n == 0) {
        return(return_network)
      }
      for (i in 1:n) {
        return_network |> nn_sum((1 / factorial(i)) |> slm(Pwr(q, eps, i))) -> return_network
        message("Power series approximation added for power: ",i)
      }
      return(return_network)
    }

    if (f == "cos") {
      1 |> slm(Pwr(q, eps, 0)) -> return_network
      if (n == 0) {
        return(return_network)
      }

      for (i in 1:n) {
        ((-1)^i) / factorial(2 * i) -> coeff
        return_network |> nn_sum(coeff |> slm(Pwr(q, eps, 2 * i))) -> return_network
        message("Power series approximation added for power: ",2*i)
      }
      return(return_network)
    }

    if (f == "sin") {
      Tay("cos", n, q, eps) -> return_network
      return_network |> comp(Aff(1, -pi / 2)) -> return_network
      return(return_network)
    }
  }
}

Vectorize(Tay) -> Tay
