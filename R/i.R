#' @title i
#' @description The function that returns the \eqn{\mathbb{i}} network.
#'
#' @param d the size of the \eqn{\mathsf{i}} network
#'
#' @return returns the i_d network
#'
#' @examples
#' i(5)
#' i(10)
#'
#' @references Definition 2.2.6. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}

i <- function(d) {
  list() -> return_network
  d |> diag() -> W
  0 |> matrix(d, 1) -> b
  list(W = W, b = b) -> return_network[[1]]
  list(W = W, b = b) -> return_network[[2]]
  return(return_network)
}
