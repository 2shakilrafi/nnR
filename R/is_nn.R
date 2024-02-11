#' @title is_nn
#' @description Function to create a list of lists for neural network layers
#'
#' @param nn A neural network. Neural networks are defined to be an ordered
#' list of ordered pairs of \eqn{(W,b)}. Where \eqn{W} is the matrix
#' representing the weight matrix \eqn{W} at that layer and \eqn{b} the bias vector.

#'
#' @return TRUE or FALSE on whether nn is indeed a neural network as defined above.
#'
#' We will use the definition of neural networks as found in:
#' @references Definition 2.1 in Rafi S., Padgett, J.L., Nakarmi, U. (2024) Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
#'
#' Which in turn is a modified version of the one found in:
#'
#' @references Definition 2.3. Grohs, P., Hornung, F., Jentzen, A. et al.
#' Space-time error estimates for deep neural network approximations
#' for differential equations. (2019).
#' \url{https://arxiv.org/abs/1908.03833}.
#'
#' @examples
#' create_nn(c(5, 6, 7)) |> is_nn()
#' Sqr(2.1, 0.1) |> is_nn()
#'
#' @export

is_nn <- function(nn) {
  # Check if it's a list
  if (!is.list(nn)) {
    return(FALSE)
  }

  # Check if it's a list of lists
  if (!all(sapply(nn, is.list))) {
    return(FALSE)
  }

  # Check if each inner list contains matrices
  all_matrices <- sapply(nn, function(layer) {
    is.list(layer) && all(sapply(layer, is.matrix))
  })

  return(all(all_matrices))
}

#'
#'
