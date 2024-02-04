#' Function to generate a random matrix with specified dimensions.
#'
#' @param rows number of rows.
#' @param cols number of columns.
#'
#' @return a random matrix of dimension rows times columns with elements from
#' a standard normal distribution

generate_random_matrix <- function(rows, cols) {
  (rows * cols) |>
    rnorm() |>
    matrix(rows, cols) -> result
  return(result)
}

#' @title create_neural_network
#' @description Function to create a list of lists for neural network layers
#'
#' @param layer_architecture a list specifying the width of each layer
#'
#' @return An ordered list of ordered pairs of \eqn{(W,b)}. Where \eqn{W} is the matrix
#' representing the weight matrix at that layer and \eqn{b} the bias vector. Entries
#' on the matrix come from a standard normal distribution. Neural networks
#' are defined to be elements belonging to the following set:
#' \deqn{
#'
#' \mathsf{NN} = \bigcup_{L\in \N} \bigcup_{l_0,l_1,...,l_L \in \N}
#' \left( \times^L_{k=1} \left[ \mathbb{R}^{l_k \times l_{k-1}} \times \R^{l_k}\right]  \right)
#'
#'
#' }
#'
#'
#'
#'
#' We will use the definition of neural networks as found in:
#'
#' @references Grohs, P., Hornung, F., Jentzen, A. et al.
#' Space-time error estimates for deep neural network approximations
#' for differential equations. Adv Comput Math 49, 4 (2023).
#' \url{https://doi.org/10.1007/s10444-022-09970-2}.
#' @export

create_neural_network <- function(layer_architecture) {
  if (all(sapply(layer_architecture, function(x) is.numeric(x) && x %% 1 == 0 && x > 0)) == FALSE) {
    stop("Non integer or negative neural network width specified.")
  } else if (layer_architecture |> length() < 2) {
    stop("Neural network must have atleast two layers.")
  } else {
    layer_architecture |> length() -> L

    # Initialize the list of lists
    neural_network <- list()

    # Generate matrices W and vectors b for each layer
    for (i in 1:(L - 1)) {
      # Set dimensions for W and b
      layer_architecture[i] -> input_size
      layer_architecture[i + 1] -> output_size

      # Create matrix W
      generate_random_matrix(output_size, input_size) -> W

      # Create vector b
      output_size |>
        rnorm() |>
        matrix(output_size, 1) -> b

      # Add W and b to the list
      list(W = W, b = b) -> neural_network[[i]]
    }

    return(neural_network)
  }
}
