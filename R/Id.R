#' @title: Id
#' @description The function that returns the \eqn{\mathsf{Id_1}} networks.
#' @param d the dimension of the \eqn{Id} network, by default it is \eqn{1}.
#'
#' @examples
#' Id()
#' Id(3)
#'
#' @references Definition 2.17. Rafi S., Padgett, J.L., Nakarmi, U. (2024)
#' Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
#'
#' @return Returns the \eqn{\mathsf{Id_1}} network.
#' @export

Id <- function(d = 1) {
  if (d %% 1 != 0 ||
    d < 1
  ) {
    stop("d must be natural numbers")
  } else if (d == 1) {
    c(1, -1) |> matrix() -> W_1
    c(0, 0) |> matrix() -> b_1
    list(W = W_1, b = b_1) -> layer_1
    c(1, -1) |> matrix(1, 2) -> W_2
    0 |> matrix() -> b_2
    list(W = W_2, b = b_2) -> layer_2
    list(layer_1, layer_2) -> result
    return(result)
  } else if (d > 1) {
    Id() -> return_network
    for (j in 2:d) {
      return_network |> stk(Id()) -> return_network
    }
    return(return_network)
  } else {
    stop("Unknown error")
  }
}
