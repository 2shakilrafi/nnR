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
#' @param q a real number in \eqn{(2,\infty)}. Accuracy as well as computation
#' time increases as \eqn{q} gets closer to \eqn{2} increases
#' @param eps a real number in \eqn{(0,\infty)}. ccuracy as well as computation
#' time increases as \eqn{\varepsilon} gets closer to \eqn{0} increases
#'
#'
#'
#' @return A neural network that takes in \eqn{x} and \eqn{y} and approximately
#' returns \eqn{xy} when instantiated with ReLU activation, and given a list
#' c(x,y), the two numbers to be multiplied.
#'
#' \emph{Note that this must be instantiated with a tuple c(x,y)}
#'
#' @references  Proposition 3.5. Grohs, P., Hornung, F., Jentzen, A. et al. Space-time error estimates for deep
#' neural network approximations for differential equations. (2019).
#' \url{https://arxiv.org/abs/1908.03833}
#'
#' @references Definition 2.25. Rafi S., Padgett, J.L., Nakarmi, U. (2024)
#' Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
#'
#' @examples
#' Prd(2.1, 0.1) |> inst(ReLU, c(4, 5)) # This may take some time, please only click once
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

Vectorize(Prd) -> Prd
