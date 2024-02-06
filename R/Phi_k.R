source("R/comp.R")
source("R/Aff.R")
source("R/i.R")
source("R/aux_fun.R")
source("R/activations.R")



#' The ck function
#'
#' @param k input value

#'
#' @return the ck function

ck <- function(k) {
  2^{
    1 - 2 * k
  } -> result
  return(result)
}

#' This is an intermediate variable, see reference.
c(0, -1 / 2, -1, 0) |> matrix() -> B


#' C_k: The function that returns the C_k matrix
#'
#' @param k Natural number, the precision with which to approximate squares
#' within \eqn{[0,1]}
#'
#' @return A neural network that approximates the square of any real within
#' \eqn{[0,1]}

C_k <- function(k) {
  c(-ck(k), 2 * ck(k), -ck(k), 1) |> matrix(1, 4) -> result
  return(result)
}


#' A_k: The function that returns the matrix A_k
#'
#' @param k Natural number, the precision with which to approximate squares
#' within \eqn{[0,1]}
#'
#' @return A neural network that approximates the square of any real within
#' \eqn{[0,1]}
#'
A_k <- function(k) {
  c(2, 2, 2, -ck(k)) |>
    c(-4, -4, -4, 2 * ck(k)) |>
    c(2, 2, 2, -ck(k)) |>
    c(0, 0, 0, 1) |>
    matrix(4, 4) -> result
  return(result)
}

#' This is an intermediate variable. See the reference
#'
c(1, 1, 1, 1) |> matrix(4, 1) -> A


#' The Phi_k function
#'
#' @param k an integer \eqn{k \in (2,\infty)}
#'
#' @return The Phi_k neural network
#' @references Grohs, P., Hornung, F., Jentzen, A. et al.
#' Space-time error estimates for deep neural network approximations
#' for differential equations. Adv Comput Math 49, 4 (2023).
#' https://doi.org/10.1007/s10444-022-09970-2
#'
Phi_k <- function(k) {
  if (k |> is.numeric() &&
    k |> length() == 1 &&
    k >= 1 &&
    k |> is.finite() &&
    k %% 1 == 0) {
    if (k == 1) {
      C_k(1) |>
        Aff(0) |>
        comp(i(4)) |>
        comp(Aff(A, B)) -> return_network
      return(return_network)
    }
    if (k >= 2) {
      C_k(k) |>
        Aff(0) |>
        comp(i(4)) -> return_network
      for (j in (k - 1):1) {
        A_k(j) |>
          Aff(B) |>
          comp(i(4)) -> intermediate_network
        return_network |> comp(intermediate_network) -> return_network
      }
      return_network |> comp(A |> Aff(B)) -> return_network
      return(return_network)
    }
  } else {
    stop("k must a natural number")
  }
}
