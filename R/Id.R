#' @title: Id
#' @description The function that returns the \eqn{\mathsf{Id_1}} networks.
#' @param d the dimension of the \eqn{Id} network, by default it is \eqn{1}.
#'
#' @return Returns the \eqn{\mathsf{Id_1}} network.
#' @export

Id <- function(d = 1) {
  if (d %% 1 != 0 ||
    d < 1
  ) {
    stop("d must be natural numbers")
  } else if (d == 1) {
    W_1 <- c(1, -1) |> matrix()
    b_1 <- c(0, 0) |> matrix()
    layer_1 <- list(W = W_1, b = b_1)
    W_2 <- c(1, -1) |> matrix(1, 2)
    b_2 <- 0 |> matrix()
    layer_2 <- list(W = W_2, b = b_2)
    result <- list(layer_1, layer_2)
    return(result)
  } else if (d > 1) {
    Id() -> return_network
    for (j in 2:d) {
      return_network |> stk(Id()) -> return_network
    }
    return(return_network)
  } else {
    stop("Unknown error")
  }
}
