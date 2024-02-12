source("R/Aff.R")
source("R/stacking.R")
source("R/comp.R")
source("R/nn_sum.R")

#' @title Nrm
#'
#' @description
#' A function that creates the \eqn{\mathsf{Nrm}} neural networks.that take
#' the 1- norm of a \eqn{d}-dimensional vector when instantiated with ReLU
#' activation.
#'
#'
#' @param d the dimensions of the vector or list being normed.
#'
#' @return a neural network that takes the 1-norm of a vector of
#' size d.under ReLU activation.

#'
#' \emph{Note:} This function is split into two cases
#' much like the definition itself.
#'
#' \emph{Note:} If you choose to specify a \eqn{d} other that \eqn{0} you must instantiate with
#' a vector or list of that length.
#'
#' For a specific definition, see:
#'
#' @references Lemma 4.2.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}
#'
#' @examples
#' Nrm(2) |> inst(ReLU, c(5,6))
#' Nrm(5) |> inst(ReLU,c(0,-9,3,4,-11))
#'
#'

#' @export
#'
Nrm <- function(d) {
  if (d %% 1 != 0 || d < 1) {
    stop("d must be a natural number")
  } else {
    if (d == 1) {
      c(1, -1) |> matrix() -> W_1
      c(0, 0) |> matrix() -> b_1
      c(1, 1) |> matrix(1, 2) -> W_2
      0 |> matrix() -> b_2

      list(W = W_1, b = b_1) -> layer_1
      list(W = W_2, b = b_2) -> layer_2

      list(layer_1, layer_2) -> return_network

      return(return_network)
    } else if (d > 1) {
      1 |> Nrm() -> first_compose
      for (i in 1:(d - 1)) {
        first_compose |> stk(Nrm(1)) -> first_compose
      }
      Sum(d, 1) |> comp(first_compose) -> return_network
      return(return_network)
    } else {
      stop("Possibly taking the norm of an invalid sized array")
    }
  }
}
