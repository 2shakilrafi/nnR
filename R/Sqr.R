source("R/comp.R")
source("R/Aff.R")
source("R/nn_sum.R")
source("R/Phi.R")
source("R/aux_fun.R")

#' @title Sqr
#' @description A function that returns the \eqn{\mathsf{Sqr}} neural networks.
#'
#' @param q parameter for the Sqr network. \eqn{2 \in (2,\infty)}
#' @param eps parameter for the Sqr network. \eqn{eps \in (0,1]}. You may
#' choose epsilon to be greater than 1 but that leads to large errors
#'
#' @return A neural network that approximates the square of a real number.when
#' provided appropriate \eqn{q,\varepsilon} and upon instantiation with ReLU,
#' and a real number \eqn{x}
#' @references  Grohs, P., Hornung, F., Jentzen, A. et al. Space-time error estimates for deep
#' neural network approximations for differential equations. Adv Comput Math 49, 4 (2023).
#' https://doi.org/10.1007/s10444-022-09970-2
#'
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
