source("R/Tay.R")

#' @title Sne
#' @description Returns the \eqn{\mathsf{Sne}} neural networks
#'
#'
#' @param n the number of Taylor iterations
#' @param q a real number in \eqn{(2,\infty)}
#' @param eps a real number in \eqn{(0,\infty)}
#'
#' \emph{Note: } In practice for most desktop uses
#' \eqn{q < 2.05} and \eqn{\varepsilon< 0.05} tends to cause problems in
#' "too long a vector", atleaast as tested on my computer.
#'
#' @return A neural network that approximates \eqn{\sin} when given
#' an appropriate \eqn{n,q,\varepsilon} and instantiated with ReLU
#' activation and given value \eqn{x}.
#'
#' @references Definition 2.30. Rafi S., Padgett, J.L., Nakarmi, U. (2024) Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
#'
#' @examples
#' Sne(3,2.1,0.1) #this may take some time, click only once and wait
#'
#' Sne(3,2.1,0.1) |> inst(ReLU,1.57) #this may take some time, click only once and wait
#'
#'
#' @export

Sne <- function(n, q, eps) {
  if (q <= 2 || eps <= 0) {
    stop("q must be > 2 and eps must be > 0")
  } else if (n %% 1 != 0 || n < 0) {
    stop("The number of Taylor iterations must be non negative integer")
  } else {
    return(Tay("sin", n, q, eps))
  }
}

Vectorize(Sne) -> Sne
