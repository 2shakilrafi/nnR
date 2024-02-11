source("R/Phi_k.R")
source("R/i.R")
source("R/Aff.R")


#' The Phi function
#'
#' @param eps parameter for Phi in \eqn{(0,\infty)}
#' @references Definition 2.23. Rafi S., Padgett, J.L., Nakarmi, U. (2024)
#' Towards an Algebraic Framework For
#' Approximating Functions Using Neural Network Polynomials
#' \url{https://arxiv.org/abs/2402.01058}
#'
#' @examples
#' Phi(0.5) |> view_nn()
#' Phi(0.1) |> view_nn()
#'
#' @return neural network Phi that approximately squares a number between
#' 0 and 1.

Phi <- function(eps) {
  if (eps |> is.numeric() &&
    eps |> length() == 1 &&
    eps |> is.finite() &&
    eps > 0) {
    (0.5 * log2(1 / eps) - 1) |> ceiling() -> M

    if (M <= 0) 1 -> M

    if (M == 1) {
      C_k(1) |>
        Aff(0) |>
        comp(i(4)) |>
        comp(Aff(A, B)) -> return_network
      return(return_network)
    }

    if (M >= 2) {
      C_k(M) |>
        Aff(0) |>
        comp(i(4)) -> return_network
      for (j in (M - 1):1) {
        A_k(j) |>
          Aff(B) |>
          comp(i(4)) -> intermediate_network
        return_network |> comp(intermediate_network) -> return_network
      }
      return_network |> comp(A |> Aff(B)) -> return_network
      return(return_network)
    }
  } else {
    stop("eps must be a positive real number")
  }
}

Vectorize(Phi) -> Phi
