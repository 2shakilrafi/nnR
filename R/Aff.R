#' @title Aff
#' @description The function that returns \eqn{\mathsf{Aff}} neural networks.
#'
#' @param W An \eqn{m \times n} matrix representing the weight of the affine
#' neural network
#' @param b An \eqn{m \times 1} vector representing the bias of the affine
#' neural network
#'
#' @references
#'
#' Definition 2.3.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}
#'
#' And especially:
#'
#' Definition 2.8. Rafi S., Padgett, J.L., Nakarmi, U. (2024) Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
#'
#' @return Returns the network \eqn{((W,b))} representing an affine neural network. Also
#' denoted as \eqn{\mathsf{Aff}_{W,b}}
#' See also \code{\link{Cpy}} and \code{\link{Sum}}.
#'
#' @examples
#' Aff(4, 5)
#' c(5, 6, 7, 8, 9, 10) |>
#'   matrix(2, 3) |>
#'   Aff(5)
#'
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
#' @references Definition 2.9. Rafi S., Padgett, J.L., Nakarmi, U. (2024) Towards an
#' Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
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
    Aff(W, b) -> return_network
    return(return_network)
  }
}

Vectorize(Cpy) -> Cpy

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
#' @references Definition 2.10. Rafi S., Padgett, J.L., Nakarmi, U. (2024) Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
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
    Aff(W, b) -> return_network

    return(return_network)
  }
}
