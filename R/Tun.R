source("R/comp.R")
source("R/Id.R")

#' Tun: The function that returns tunneling neural networks
#'
#' @param n The depth of the tunnel network where \eqn{n \in \mathbb{N} \cap [1,\infty)}.
#' @param d The dimension of the tunneling network. By default it is assumed to be \eqn{1}.
#'
#' @return A tunnel neural network of depth n. A tunneling neural
#' network is defined as the neural network \eqn{\mathsf{Aff}_{1,0}} for \eqn{n=1},
#' the neural network \eqn{\mathsf{Id}_1} for \eqn{n=1} and the neural network
#' \eqn{\bullet^{n-2}\mathsf{Id}_1} for \eqn{n >2}. For this to work we
#' must provide an appropriate \eqn{n} and instantiate with ReLU at some
#' real number \eqn{x}.
#' @export
#'
Tun <- function(n, d = 1) {
  if (n %% 1 != 0 ||
    n < 1 ||
    d %% 1 != 0 ||
    d < 1
  ) {
    stop("n and d must be natural numbers")
  }
  if (d == 1) {
    if (n == 1) {
      return(Aff(1, 0))
    } else if (n == 2) {
      return(Id())
    } else if (n > 2) {
      Id() -> return_network
      for (i in 3:n) {
        return_network |> comp(Id()) -> return_network
      }
      return(return_network)
    }
  } else if (d > 1) {
    if (n == 1) {
      return(Aff(diag(d), 0 |> matrix()))
    } else if (n == 1) {
      return(Id(d))
    } else if (n == 2) {
      return(Id(d))
    } else if (n > 2) {
      Id(d) -> return_network
      for (i in 3:n) {
        return_network |> comp(Id(d)) -> return_network
      }
      return(return_network)
    }
  } else {
    stop("Unknown error")
  }
}
