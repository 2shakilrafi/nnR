source("R/Tay.R")

#' The Xpn function
#'
#' @param n The number of Taylor iterations. Accuracy as well as computation
#' time increases as \eqn{n} increases
#' @param q a real number in \eqn{(2,\infty)}. Accuracy as well as computation
#' time increases as \eqn{q} gets closer to \eqn{2} increases
#' @param eps a real number in \eqn{(0,\infty)}. ccuracy as well as computation
#' time increases as \eqn{\varepsilon} gets closer to \eqn{0} increases
#'
#' \emph{Note: } In practice for most desktop uses
#' \eqn{q < 2.05} and \eqn{\varepsilon< 0.05} tends to cause problems in
#' "too long a vector", atleaast as tested on my computer.
#'
#' @return A neural network that approximates \eqn{e^x} for real \eqn{x} when
#' given appropriate \eqn{n,q,\varepsilon} and instnatiated with ReLU
#' activation at point\eqn{x}.
#'
#' @references Definition 2.28 in Rafi S., Padgett, J.L., Nakarmi, U. (2024) Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
#'
#' @examples
#' Xpn(3, 2.25, 0.25) # this may take some time
#'
#' @examples
#' Xpn(3, 2.2, 0.2) |> inst(ReLU, 1.5) # this may take some time
#'
#' @export

Xpn <- function(n, q, eps) {
  if (q <= 2 || eps <= 0) {
    stop("q must be > 2 and eps must be > 0")
  } else if (n %% 1 != 0 || n < 0) {
    stop("The number of Taylor iterations must be a non negative integer")
  } else {
    return(Tay("exp", n, q, eps))
  }
}

Vectorize(Xpn) -> Xpn
