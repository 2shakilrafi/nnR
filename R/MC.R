source("R/stacking.R")
source("R/comp.R")
source("R/Nrm.R")
source("R/Mxm.R")

#' The P neural network
#'
#' @param X a matrix. More precisely a collection of n row vectors of size d.
#' That is to say an d by n matrix
#' @param y a row vector of size m that is the ouput of the function at each
#' of the n sample points
#' @param L the Lipschitz constant for the function.
#'
#' @references Lemma 4.2.9. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}.
#'
#' @return A neural network that gives the maximum convolution approximation
#' of a function whose outputs is \eqn{y} at \eqn{n} sample points given by
#' each row of \eqn{X}, when instantiated with ReLU.
#' @export

P <- function(X, y, L) {
  if (X |> is.matrix() == FALSE) (X |> matrix() -> X)
  if (y |> is.matrix() == FALSE) (y |> matrix() -> y)
  X |> nrow() -> d # the dimensionality of our x samples
  X |> ncol() -> n # the number of samples to be taken

  if (n == 1) {
    return("Enter atleast 2 interpolating points")
  }
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
