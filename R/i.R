#' @title: i
#' @description The function that returns the \eqn{\mathbb{i}} network.
#'
#' @param d the size of the \eqn{\mathsf{i}} network
#'
#' @return returns the i_d network

i <- function(d) {
  list() -> return_network
  d |> diag() -> W
  0 |> matrix(d, 1) -> b
  list(W = W, b = b) -> return_network[[1]]
  list(W = W, b = b) -> return_network[[2]]
  return(return_network)
}
