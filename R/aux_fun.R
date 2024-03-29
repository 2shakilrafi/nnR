source("R/is_nn.R")
#' @title hid
#'
#' @description The function that returns the number of hidden layers of a
#' neural network. Denoted \eqn{\mathsf{H}}
#'
#' @param nu a neural network of the type generated by create_nn()
#'
#' By definition \eqn{\mathsf{H}(\nu) = \mathsf{D}(\nu) - 1}
#'
#' @references Definition 1.3.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}.
#'
#' @return Integer representing the number of hidden layers.
#' @examples
#' create_nn(c(4, 5, 6, 2)) |> hid()
#'
#' @export

hid <- function(nu) {
  if (nu |> is_nn() == TRUE) {
    return(length(nu) - 1)
  } else {
    stop("Only neural networks can have hidden layers")
  }
}



#' @title dep
#' @description The function that returns the depth of a neural network. Denoted
#' \eqn{\mathsf{D}}.
#'
#' @param nu a neural network of the type generated by
#' create_nn(). Very straightforwardly it is the
#' length of the list where neural networks are defined as an odered list of
#' lists.
#' @references Definition 1.3.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}.
#'
#' @return Integer representing the depth of the neural network.
#'
#' @examples
#' create_nn(c(4, 5, 6, 2)) |> dep()
#' @export

dep <- function(nu) {
  if (nu |> is_nn() == TRUE) {
    return(length(nu))
  } else {
    stop("Only neural networks can have depth")
  }
}



#' @title inn
#' @description The function that returns the input layer size of a neural
#' network. Denoted \eqn{\mathsf{I}}
#'
#' @param nu A neural network of the type generated by
#' create_nn().
#'
#' @references Definition 1.3.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}.
#'
#' @return An integer representing the input width of the neural network.
#' @examples
#' create_nn(c(4, 5, 6, 2)) |> inn()
#' @export

inn <- function(nu) {
  if (nu |> is_nn() == TRUE) {
    return(dim(nu[[1]]$W)[2])
  } else {
    stop("Only neural networks can have size of input layers")
  }
}



#' @title out
#' @description The function that returns the output layer size of a neural
#' network. Denoted \eqn{\mathsf{O}}.
#'
#' @param nu A neural network of the type generated by create_nn().
#'
#' @references Definition 1.3.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}.
#'
#' @return An integer representing the output width of the neural network.
#' @examples
#' create_nn(c(4, 5, 6, 2)) |> out()
#' @export

out <- function(nu) {
  if (nu |> is_nn() == TRUE) {
    return(dim(nu[[length(nu)]]$W)[1])
  } else {
    stop("Ony neural networks can have size of output layers")
  }
}



#' @title lay
#' @description The function that returns the layer architecture of a neural
#' network.
#'
#' @param nu A neural network of the type generated by
#' create_nn(). Denoted \eqn{\mathsf{L}}.
#'
#' @references Definition 1.3.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}.
#'
#' @return A tuple representing the layer architecture of our neural network.
#' @examples
#' create_nn(c(4, 5, 6, 2)) |> lay()
#' @export


lay <- function(nu) {
  if (is_nn(nu)) {
    layer_architecture <- sapply(nu, function(x) dim(x$W)[1])
    layer_architecture <- c(inn(nu),layer_architecture)
    return(layer_architecture)
  } else {
    stop("Only neural networks can have layer architectures")
  }
}



#' @title param
#' @description The function that returns the numbe of parameters of a neural
#' network.
#'
#' @param nu A neural network of the type generated by
#' create_nn(). Denoted \eqn{\mathsf{P}}.
#'
#' @references Definition 1.3.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}.
#'
#' @return An integer representing the parameter count of our neural network.
#' @examples
#' create_nn(c(4, 5, 6, 2)) |> param()
#' @export

param <- function(nu) {
  if (is_nn(nu)) {
    param_count <- sum(sapply(nu, function(x) length(x$W) + length(x$b)))
    return(param_count)
  } else {
    stop("Only neural networks can have parameters")
  }
}
