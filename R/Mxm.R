source("R/Aff.R")
source("R/stacking.R")
source("R/comp.R")
source("R/nn_sum.R")
source("R/Id.R")

#' @title Mxm
#' @description The function that returns the \eqn{\mathsf{Mxm}} neural networks.
#'
#' \emph{Note:} Because of certain quirks of R we will have split
#' into five cases. We add an extra case for \eqn{d = 3}. Unlike the paper
#' we will simply reverse engineer the appropriate \emph{d}.
#'
#' @param d The dimension of the input vector on instantiation.
#'
#' @examples
#' Mxm(1) |> inst(ReLU, -5)
#' Mxm(3) |> inst(ReLU, c(4, 5, 1))
#' Mxm(5) |> inst(ReLU, c(5, 3, -1, 6, 6))
#'
#' @return The neural network that will ouput the maximum of a vector of
#' size \eqn{d} when activated with the ReLU function.
#'
#' For a specific definition, see:
#' @references Lemma 4.2.4. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}


#' @export

Mxm <- function(d) {
  if (d %% 1 != 0 || d < 1) {
    stop("d must be a natural number")
  }
  if (d == 1) {
    return(Aff(1, 0))
  } else if (d == 2) {
    c(1, 0, 0, -1, 1, -1) |> matrix(3, 2) -> W_1
    c(0, 0, 0) |> matrix() -> b_1
    c(1, 1, -1) |> matrix(1, 3) -> W_2
    0 |> matrix() -> b_2
    list(W = W_1, b = b_1) -> layer_1
    list(W = W_2, b = b_2) -> layer_2
    list(layer_1, layer_2) -> return_network
    return(return_network)
  } else if (d == 3) {
    Mxm(2) |> stk(Id()) -> first_compose
    Mxm(2) |> comp(first_compose) -> return_network
    return(return_network)
  } else if ((d %% 2 == 0) & (d > 3)) {
    d / 2 -> d
    Mxm(2) -> first_compose
    for (i in 1:(d - 1)) {
      first_compose |> stk(Mxm(2)) -> first_compose
    }
    Mxm(d) |> comp(first_compose) -> return_network
    return(return_network)
  } else if ((d %% 2 != 0) & (d > 3)) {
    (d - 1) / 2 -> d

    Mxm(2) -> first_compose
    for (i in 1:(d - 1)) {
      first_compose |> stk(Mxm(2)) -> first_compose
    }
    first_compose |> stk(Id()) -> first_compose
    Mxm(d + 1) |> comp(first_compose) -> return_network
    return(return_network)
  } else {
    stop("Possibly taking max of vector of length 0")
  }
}
