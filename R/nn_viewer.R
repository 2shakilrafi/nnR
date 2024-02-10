source("R/is_nn.R")
source("R/aux_fun.R")


#' view_nner
#'
#' @param nn A neural network of the type creted by \code{\link{create_nn()}}, i.e.
#' a list of lists of \eqn{W} and \eqn{b}.
#'
#' @return A displayed version of the neural network. This may be required if
#' the neural network is very deep.
#' @export
#'
#' @examples
#' Xpn(3,2.1,1.1) |> view_nn()
#' Pwr(2,2.1,0.1) |> view_nn()
view_nn <- function(nn) {
  if (nn |> is_nn() == TRUE) {
    for (i in 1:dep(nn)) {
      print(nn[i])
    }
  }
}
