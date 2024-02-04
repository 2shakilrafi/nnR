source("R/Aff.R")
source("R/comp.R")
source("R/nn_sum.R")
source("R/Phi.R")
source("R/scalar_mult.R")

#' @title Prd
#' @description
#' A function that returns the \eqn{\mathsf{Prd}} neural networks that
#' approximates the product of two real numbers when given an appropriate
#' \eqn{q}, \eqn{\varepsilon}, a real number \eqn{x} and instantiation with ReLU.
#' activation.
#'
#' @param q A real number within \eqn{(2,\infty)}
#' @param eps A real number within \eqn{(0,\infty)}
#'
#' @return A neural network that takes in \eqn{x} and \eqn{y} and approximately
#' returns \eqn{xy} when instantiated with ReLU activation, and given a list
#' c(x,y), the two numbers to be multiplied.
#'
#' @references  Grohs, P., Hornung, F., Jentzen, A. et al. Space-time error estimates for deep
#' neural network approximations for differential equations. Adv Comput Math 49, 4 (2023).
#' https://doi.org/10.1007/s10444-022-09970-2
#'
#'
#' @export

Prd <- function(q, eps) {
  if (q <= 2 || eps <= 0) {
    stop("q must be > 2 and eps must be > 0")
  } else {
    c(1, 1) |> matrix(1, 2) -> A_1
    c(1, 0) |> matrix(1, 2) -> A_2
    c(0, 1) |> matrix(1, 2) -> A_3

    0.5 |>
      slm(Sqr(q, eps)) |>
      comp(Aff(A_1, 0)) |>
      nn_sum(-0.5 |> slm(Sqr(q, eps)) |> comp(Aff(A_2, 0))) |>
      nn_sum(-0.5 |> slm(Sqr(q, eps)) |> comp(Aff(A_3, 0))) -> return_network
    return(return_network)
  }
}
