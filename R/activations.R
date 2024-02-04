#' @title: ReLU
#' @description: The ReLU activation function
#'
#' @param x A real number that is the input to our ReLU function.
#'
#' @return The output of the standard ReLU function, i.e. \eqn{\max\{0,x\}}. See also \code{\link{Sigmoid}}.
#' and \code{\link{Tanh}}.
#' @export

ReLU <- function(x) {
  if (x |> is.numeric() && x |> length() == 1 && x |> is.finite()) {
    return(x |> max(0))
  } else {
    stop("x must be a real number")
  }
}

#' @title: Sigmoid
#' @description The Sigmoid activation function.
#'
#' @param x a real number that is the input to our Sigmoid function
#'
#' @return The output of a standard Sigmoid function,
#' i,e. \eqn{\frac{1}{1 + \exp(-x)}}.
#' See also \code{\link{Tanh}}.and \code{\link{ReLU}}.
#' @export

Sigmoid <- function(x) {
  if (x |> is.numeric() && x |> length() == 1 && x |> is.finite()) {
    return(1 / (1 + exp(-x)))
  } else {
    stop("x must be a real number")
  }
}

#' @title Tanh
#' @description The tanh activation function
#'
#' @param x a real number
#'
#' @return the \eqn{tanh} of x. See also \code{\link{Sigmoid}} and
#' \code{\link{ReLU}}.
#' @export

Tanh <- function(x) {
  if (x |> is.numeric() && x |> length() == 1 && x |> is.finite()) {
    return(x |> tanh())
  } else {
    stop("x must be a real number")
  }
}
