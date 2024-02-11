#' @title Trp
#' @description The function that returns the \eqn{\mathsf{Trp}} networks.
#'
#' @param h the horizontal distance between two mesh points
#'
#' @return The \eqn{\mathsf{Trp}} network that gives the area
#' when activated with ReLU or any continuous function and two
#' meshpoint values \eqn{x_1} and \eqn{x_2}.
#'
#' @examples
#' Trp(0.1)
#' Trp(0.5) |> inst(ReLU, c(9, 7))
#' Trp(0.1) |> inst(Sigmoid, c(9, 8))
#'
#' @references Definition 2.31. Rafi S., Padgett, J.L., Nakarmi, U. (2024)
#' Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
#'
#'
#' @export

Trp <- function(h) {
  if (h |> is.numeric() &&
    h |> length() == 1 &&
    h |> is.finite() &&
    h > 0) {
    c(h / 2, h / 2) |> matrix(1, 2) -> W
    0 |> matrix() -> b
    list(list(W = W, b = b)) -> return_network
    return(return_network)
  } else {
    stop("h must be a positive real number")
  }
}

#' @title Etr
#' @description The function that returns the \eqn{\mathsf{Etr}} networks.
#'
#' @param n number of trapezoids to make. Note this will result in a set of
#' trapezoids. A natural number.
#'
#' @param h width of trapezoids. A positive real number.
#'
#'
#' \emph{Note: } Upon instantiation with any continuous function this neural
#' network must be fed with \eqn{n+1} real numbers representing the values
#' of the function being approximated at the \eqn{n+1} meshpoints which are
#' the legs of the \eqn{n} trapezoids as stipulated in the input parameter \eqn{n}..
#'
#' @examples
#' Etr(5, 0.1)
#' seq(0, pi, length.out = 1000) |> sin() -> samples
#' Etr(1000 - 1, pi / 1000) |> inst(ReLU, samples)
#'
#' seq(0, 2, length.out = 1000)^2 -> samples
#' Etr(1000 - 1, 2 / 1000) |> inst(Tanh, samples)
#'
#' @references Definition 2.33. Rafi S., Padgett, J.L., Nakarmi, U. (2024)
#' Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
#'
#'
#' @return An approximation for value of the integral of a function. Must be instantiated
#' with a list of \eqn{n+1} reals
#' @export

Etr <- function(n, h) {
  if (h |> is.numeric() &&
    h |> length() == 1 &&
    h |> is.finite() &&
    h > 0 &&
    n %% 1 == 0 &&
    n > 1) {
    c(h / 2, rep(h, n - 1), h / 2) |>
      matrix() |>
      t() -> W
    0 |> matrix() -> b
    list(list(W = W, b = b)) -> return_network
    return(return_network)
  } else {
    stop("n must be a natural number and h must be a positive real number.")
  }
}
