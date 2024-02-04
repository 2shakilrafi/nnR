#' @title Trp
#' @description The function that returns the \eqn{\mathsf{Trp}} networks.
#'
#' @param h the horizontal distance between two mesh points
#'
#' @return The \eqn{\mathsf{Trp}} network that gives the area
#' when activated with ReLU and two meshpoint values x_1 and x_2.
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
#' trapezoids.
#' Note that this will result in n+1 meshpoints including the starting a and
#' ending b
#'
#' \emph{Note: } Upon instantiation with any continuous function this neural
#' network must be fed with \eqn{n+1} real numbers representing the values
#' of the function being approximated at the \eqn{n+1} meshpoints which are
#' the legs of the \eqn{n} triangles as stipulated in the input parameters.
#'
#' @param h width of trapezoids
#'
#' @return an approximation for area of the integral
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
