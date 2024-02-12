source("R/aux_fun.R")
source("R/is_nn.R")


#' @title comp
#' @description The function that takes the composition of two neural
#' networks assuming they are compatible, i.e., given
#' \eqn{\nu_1, \nu_2 \in \mathsf{NN}}, it must be the case that
#' \eqn{\mathsf{I}(\nu)_1 = \mathsf{O}(\nu_2)}.
#'
#' @param phi_1 first neural network to be composed, goes on the left
#' @param phi_2 second neural network to be composed, goes on right
#'
#' @return The composed neural network. See also \code{\link{dep}}.
#'
#'
#' Our definition derive specifically from:
#' @references Definition 2.1.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
#' Mathematical introduction to deep learning: Methods, implementations,
#' and theory. \url{https://arxiv.org/abs/2310.20360}
#'
#' \emph{Remark:} We have two versions of this function, an
#' infix version for close resemblance to mathematical notation and
#' prefix version.
#'
#' @examples
#' create_nn(c(5, 4, 6, 7)) |> comp(create_nn(c(4, 1, 5)))

#' @encoding utf8
#' @export
#'


comp <- function(phi_1, phi_2) {
  if (phi_1 |> is_nn() && phi_2 |> is_nn()) {
    dep(phi_1) -> L
    dep(phi_2) -> L_

    if (L > 1 & L_ > 1) {
      phi_2[-L_] -> beginning
      phi_1[-1] -> end
      phi_1[[1]]$W %*% phi_2[[L_]]$W -> mid_W
      phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b -> mid_b
      list(W = mid_W, b = mid_b) -> mid
      c(
        beginning,
        list(mid),
        end
      ) -> composed_network
      return(composed_network)
    } else if (L > 1 & L_ == 1) {
      phi_1[[1]]$W %*% phi_2[[1]]$W -> beginning_W
      phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b -> beginning_b
      list(
        W = beginning_W,
        b = beginning_b
      ) -> beginning
      phi_1[-1] -> end
      c(
        list(beginning),
        end
      ) -> composed_network
      return(composed_network)
    } else if (L == 1 & L_ > 1) {
      phi_2[-L_] -> beginning
      phi_1[[1]]$W %*% phi_2[[L_]]$W -> end_W
      phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b -> end_b
      list(
        W = end_W,
        b = end_b
      ) -> end
      c(
        beginning,
        list(end)
      ) -> composed_network
      return(composed_network)
    } else if (L == 1 & L_ == 1) {
      list() -> composed_network
      phi_1[[1]]$W %*% phi_2[[1]]$W -> W
      phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b -> b
      list(
        W = W,
        b = b
      ) -> composed_network[[1]]
      return(composed_network)
    } else {
      stop("Dimensionality mismatch")
    }
  } else {
    stop("Only neural networks can be composed.")
  }
}



#' The `infix version of comp function
#'
#' @param phi_1 first neural network to be composed, goes on the left
#' @param phi_2 second neural network to be composed, goes on right
#'
#' @rdname comp
#' @export


`%comp%` <- function(phi_1, phi_2) {
  if (phi_1 |> is_nn() && phi_2 |> is_nn()) {
    dep(phi_1) -> L
    dep(phi_2) -> L_

    if (L > 1 & L_ > 1) {
      phi_2[-L_] -> beginning
      phi_1[-1] -> end
      phi_1[[1]]$W %*% phi_2[[L_]]$W -> mid_W
      phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b -> mid_b
      list(W = mid_W, b = mid_b) -> mid
      c(
        beginning,
        list(mid),
        end
      ) -> composed_network
      return(composed_network)
    } else if (L > 1 & L_ == 1) {
      phi_1[[1]]$W %*% phi_2[[1]]$W -> beginning_W
      phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b -> beginning_b
      list(
        W = beginning_W,
        b = beginning_b
      ) -> beginning
      phi_1[-1] -> end
      c(
        list(beginning),
        end
      ) -> composed_network
      return(composed_network)
    } else if (L == 1 & L_ > 1) {
      phi_2[-L_] -> beginning
      phi_1[[1]]$W %*% phi_2[[L_]]$W -> end_W
      phi_1[[1]]$W %*% phi_2[[L_]]$b + phi_1[[1]]$b -> end_b
      list(
        W = end_W,
        b = end_b
      ) -> end
      c(
        beginning,
        list(end)
      ) -> composed_network
      return(composed_network)
    } else if (L == 1 & L_ == 1) {
      list() -> composed_network
      phi_1[[1]]$W %*% phi_2[[1]]$W -> W
      phi_1[[1]]$W %*% phi_2[[1]]$b + phi_1[[1]]$b -> b
      list(
        W = W,
        b = b
      ) -> composed_network[[1]]
      return(composed_network)
    } else {
      stop("Dimensionality mismatch")
    }
  } else {
    stop("Only neural networks can be composed.")
  }
}
