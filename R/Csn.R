source("R/Tay.R")

#' @title Csn
#' @description The function that returns \eqn{\mathsf{Csn}}.
#'
#' @param n The number of Taylor iterations
#' @param q a real number in \eqn{(2,\infty)}
#' @param eps a real number in \eqn{(0,\infty)}
#'
#' \emph{Note: } In practice for most desktop uses
#' \eqn{q < 2.05} and \eqn{\varepsilon< 0.05} tends to cause problems in
#' "too long a vector", atleaast as tested on my computer.
#'
#' @return A neural network that approximates \eqn{\cos} under instantiation
#' with ReLU activation. See also \code{\link{Sne}}.
#' @export

Csn <- function(n, q, eps) {
  if (q <= 2 || eps <= 0) {
    stop("q must be > 2 and eps must be > 0")
  } else if (n %% 1 != 0 || n < 0) {
    stop("The number of Taylor iterations must be non negative integer")
  } else {
    Tay("cos", n, q, eps) -> return_network
    return(return_network)
  }
}
