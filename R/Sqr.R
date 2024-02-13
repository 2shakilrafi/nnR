source("R/comp.R")
source("R/Aff.R")
source("R/nn_sum.R")
source("R/Phi.R")
source("R/aux_fun.R")

#' @title Sqr
#' @description A function that returns the \eqn{\mathsf{Sqr}} neural networks.
#'
#' @param q a real number in \eqn{(2,\infty)}. Accuracy as well as computation
#' time increases as \eqn{q} gets closer to \eqn{2} increases
#' @param eps a real number in \eqn{(0,\infty)}. ccuracy as well as computation
#' time increases as \eqn{\varepsilon} gets closer to \eqn{0} increases
#'
#' @return A neural network that approximates the square of a real number.when
#' provided appropriate \eqn{q,\varepsilon} and upon instantiation with ReLU,
#' and a real number \eqn{x}
#' @references  Proposition 3.4. Grohs, P., Hornung, F., Jentzen, A. et al. Space-time error estimates for deep
#' neural network approximations for differential equations. (2019).
#' \url{https://arxiv.org/abs/1908.03833}
#'
#' #' @references Definition 2.24. Rafi S., Padgett, J.L., Nakarmi, U. (2024)
#' Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
#'
#' @examples
#' Sqr(2.5, 0.1)
#' Sqr(2.5, 0.1) |> inst(ReLU, 4)
#'
#' @export


Sqr <- function(q, eps) {
  if (q <= 2 || eps <= 0) {
    stop("q must be > 2 and eps must be > 0")
  } else {
    2^(-2 / (q - 2)) * eps^(q / (q - 2)) -> delta
    (eps / 2)^(1 / (q - 2)) -> alpha

    (0.5 * log2(1 / eps) - 1) |> ceiling() -> M

    if (M <= 0) 1 else M -> M

    (Aff(alpha^(-2), 0) |> comp(Phi(delta))) |>
      comp(Aff(alpha, 0)) -> first_summand

    (Aff(alpha^(-2), 0) |> comp(Phi(delta))) |>
      comp(Aff(-alpha, 0)) -> second_summand

    first_summand |>
      nn_sum(second_summand) -> return_network

    return(return_network)
  }
}

Vectorize(Sqr) -> Sqr
