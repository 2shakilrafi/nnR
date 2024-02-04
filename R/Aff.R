#' @title Aff
#' @description The function that returns \eqn{\mathsf{Aff}} neural networks.
#'
#' @param W An \eqn{m \times n} matrix representing the weight of the affine
#' neural network
#' @param b An \eqn{m \times 1} vector representing the bias of the affine
#' neural network
#'
#' @references  Grohs, P., Hornung, F., Jentzen, A. et al. Space-time error estimates for deep
#' neural network approximations for differential equations. Adv Comput Math 49, 4 (2023).
#' https://doi.org/10.1007/s10444-022-09970-2
#'
#' Definition 2.3.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}
#'
#' @return Returns the network \eqn{((W,b))} representing an affine neural network. Also
#' denoted as \eqn{\mathsf{Aff}_{W,b}}
#' See also \code{\link{Cpy}} and \code{\link{Sum}}.
#' @export

Aff <- function(W, b) {
  if (W |> is.matrix() == FALSE) (W |> matrix() -> W)
  if (b |> is.matrix() == FALSE) (b |> matrix() -> b)
  list(list(W = W, b = b)) -> return_network
  return(return_network)
}

#' @title Cpy
#' @description The function that returns \eqn{\mathsf{Cpy}} neural networks.
#' These are neural networks defined as such
#' \deqn{
#' \mathsf{Aff}_{\left[ \mathbb{I}_k \: \mathbb{I}_k \: \cdots \: \mathbb{I}_k\right]^T,0_{k}}
#' }
#'
#' @param n number of copies to make.
#' @param k the size of the input vector.
#'
#' @return Returns an affine network that makes a concatenated vector that is \eqn{n}
#' copies of the input vector of size \eqn{k}. See \code{\link{Aff}} and \code{\link{Sum}}.
#'
#' @references Definition 2.4.6. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}
#'
#'
#' @export

Cpy <- function(n, k) {
  if (n %% 1 != 0 ||
    n < 1 ||
    k %% 1 != 0 ||
    k < 1) {
    stop("n and k must be natural numbers")
  } else {
    k |> diag() -> W
    for (i in 2:n) {
      W |> rbind(k |> diag()) -> W
    }
    0 |> matrix(n * k) -> b
    list(list(W = W, b = b)) -> return_network
    return(return_network)
  }
}

#' @title Sum
#' @description The function that returns \eqn{\mathsf{Sum}} neural networks.
#'
#' These are neural networks defined as such
#' \deqn{
#' \mathsf{Aff}_{\left[ \mathbb{I}_k \: \mathbb{I}_k \: \cdots \: \mathbb{I}_k\right],0_{k}}
#' }
#'
#' @param n number of copies of a certain vector to be summed.
#' @param k the size of the summation vector.
#'
#' @return An affine neural network that will take a vector of size
#' \eqn{n \times k} and return the summation vector that is of length
#' \eqn{k}. See also \code{\link{Aff}} and \code{\link{Cpy}}.
#'
#' @references Definition 2.4.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}
#'
#'
#' @export
#'


Sum <- function(n, k) {
  if (n %% 1 != 0 ||
    n < 1 ||
    k %% 1 != 0 ||
    k < 1) {
    stop("n and k must be natural numbers")
  } else {
    k |> diag() -> W
    for (i in 2:n) {
      W |> cbind(k |> diag()) -> W
    }
    0 |> matrix(k) -> b
    list(list(W = W, b = b)) -> return_network

    return(return_network)
  }
}
