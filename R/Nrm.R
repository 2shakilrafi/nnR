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
#' @param d the dimensions of the vector being normed.
#'
#' @return a neural network that takes the 1-norm of a vector of
#' size d.under ReLU activation. This is the neural network that is:
#' \deqn{
#' \mathsf{Nrm}^1_1 = \left( \left( \begin{bmatrix} 1 \\ -1\end{bmatrix},
#' \begin{bmatrix} 0 \\ 0 \end{bmatrix}\right), \left( \begin{bmatrix}1 && 1\end{bmatrix},
#' \begin{bmatrix}0\end{bmatrix}\right) \right) \in \left( \mathbb{R}^{2 \times 1} \times
#' \mathbb{R}^2 \right) \times \left( \mathbb{R}^{1 \times 2} \times \mathbb{R}^1 \right) \quad d=1 \\
#' \mathsf{Nrm}_1^d = \mathsf{Sum}_{d,1} \bullet \left[ \boxminus_{i=1}^d \mathsf{Nrm}_1^1 \right] \quad d>1

#' }
#'
#'
#'
#' \emph{Note:} This function is split into two cases
#' much like the definition itself.
#'
#' @references Lemma 4.2.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}

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
