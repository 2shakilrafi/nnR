source("R/Phi_k.R")
source("R/i.R")
source("R/Aff.R")


#' The Phi function
#'
#' @param eps parameter for Phi
#' @references Grohs, P., Hornung, F., Jentzen, A. et al.
#' Space-time error estimates for deep neural network approximations
#' for differential equations. Adv Comput Math 49, 4 (2023).
#' https://doi.org/10.1007/s10444-022-09970-2
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
