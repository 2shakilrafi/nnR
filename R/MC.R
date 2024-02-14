source("R/stacking.R")
source("R/comp.R")
source("R/Nrm.R")
source("R/Mxm.R")

#' The MC neural network
#'
#' @description
#' This function implements the 1-D approximation scheme outlined in the References.
#'
#' \strong{Note:} Only 1-D interpolation is implemented.
#'
#' @param X a list of samples from the functions domain.
#' @param y the function applied componentwise to each point in the domain.
#' @param L the Lipschitz constant for the function. Not necessarily global,
#' but could be an absolute upper limit of slope, over the domain.
#'
#' @examples
#'
#' seq(0, 3.1416, length.out = 200) -> X
#' sin(X) -> y
#' MC(X, y, 1) |> inst(ReLU, 0.25) # compare to sin(0.25)
#'
#' @references Lemma 4.2.9. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}.
#'
#' @return A neural network that gives the maximum convolution approximation
#' of a function whose outputs is \eqn{y} at \eqn{n} sample points given by
#' each row of \eqn{X}, when instantiated with ReLU.
#' @export

MC <- function(X, y, L) {
  if (X |> is.matrix() == FALSE) {
    X |>
      matrix() |>
      t() -> X
    message("X was automatically turned into a row vector.")
  }
  if (y |> is.matrix() == FALSE) {
    y |> matrix() -> y
    message("y was automatically turned into a column vector.")
  }
  X |> nrow() -> d # the dimensionality of our x samples
  X |> ncol() -> n # the number of samples to taken
  if (n == 1) {
    stop("Enter atleast 2 interpolating points")
  } else if (ncol(X) != nrow(y)) {
    stop("X and y mismatch")
  }
  else if (nrow(X) > 1) {
    stop("Only 1-D interpolation is implemented")
  }
  else {
    Cpy(n, d) -> first_compose # the first neural network to be hit with x
    Nrm(d) |> comp(Aff(diag(d), -X[, 1])) -> second_compose # the second neural network to be hit
    for (j in 2:n) {
      second_compose |> stk(Nrm(d) |> comp(Aff(diag(d), -X[, j]))) -> second_compose
    }
    Aff(-L * diag(n), y) -> third_compose # the third neural network to be hit
    n |> Mxm() -> fourth_compose # the fourth neural network to be hit
    fourth_compose |>
      comp(third_compose) |>
      comp(second_compose) |>
      comp(first_compose) -> return_network
    # the final neural network
    return(return_network)
  }
}
