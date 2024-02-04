#' @title is_nn
#' @description Function to create a list of lists for neural network layers
#'
#' @param nn A neural network. Neural networks are defined to be an ordered
#' list of ordered pairs of \eqn{(W,b)}. Where \eqn{W} is the matrix
#' representing the weight matrix at that layer and \eqn{b} the bias vector.
#' Neural networks are defined to be elements belonging to the following set:
#' \deqn{
#' \mathsf{NN} = \bigcup_{L\in \N} \bigcup_{l_0,l_1,...,l_L \in \N}
#' \left( \times^L_{k=1} \left[ \mathbb{R}^{l_k \times l_{k-1}} \times \R^{l_k}\right]  \right)
#' }
#'
#' See references for a more detailed discussion.
#'
#' @return TRUE or FALSE on whether nn is indeed a neural network as defined above.
#'
#' We will use the definition of neural networks as found in the references section.#'
#' @references Grohs, P., Hornung, F., Jentzen, A. et al.
#' Space-time error estimates for deep neural network approximations
#' for differential equations. Adv Comput Math 49, 4 (2023).
#' \url{https://doi.org/10.1007/s10444-022-09970-2}.
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
#'
