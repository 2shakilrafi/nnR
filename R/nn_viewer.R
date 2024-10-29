source("R/is_nn.R")
source("R/aux_fun.R")


#' @title view_nn
#'
#' @description Takes a neural network shown in vectorized form and explicitly
#' displays it.
#'
#' @param nn A neural network., i.e.
#' a list of lists of \eqn{W} and \eqn{b}.
#'
#' @examples
#' c(5, 6, 7, 9) |>
#'   create_nn() |>
#'   view_nn()
#' Sqr(2.1, 0.1) |> view_nn()
#'
#' @return A displayed version of the neural network. This may be required if
#' the neural network is very deep.
#' @export
#'
#' @examples
#' Xpn(3, 2.1, 1.1) |> view_nn()
#' Pwr(2.1, 0.1, 3) |> view_nn()
view_nn <- function(nn) {
  if (nn |> is_nn() == TRUE) {
    for (i in 1:dep(nn)) {
      print(nn[i])
    }
  } else {
    stop("You may only view neural networks")
  }
}

draw_nn <- function(layers) {

  if (layers |> is.list() == TRUE && all(sapply(x, is.numeric)) == TRUE) {

    n_layers <- length(layers)
    x_range <- c(0, n_layers + 1)
    y_range <- c(0, 1)

    # Set up the plot area
    plot(NA, xlim = x_range, ylim = y_range, type = 'n', xaxt = 'n', yaxt = 'n',
         xlab = '', ylab = '', bty = 'n', main = 'Neural Network')

    node_coords <- list()

    # Calculate positions and draw nodes
    for (i in 1:n_layers) {
      n_nodes <- layers[i]
      x <- i
      if (n_nodes > 1) {
        y_positions <- seq(from = y_range[1] + 0.1, to = y_range[2] - 0.1, length.out = n_nodes)
      } else {
        y_positions <- mean(y_range)
      }
      node_coords[[i]] <- data.frame(x = rep(x, n_nodes), y = y_positions)

      # Draw nodes
      points(x = rep(x, n_nodes), y = y_positions, pch = 21, bg = 'white', cex = 2)
    }

    # Draw connections between nodes
    for (i in 1:(n_layers - 1)) {
      nodes_from <- node_coords[[i]]
      nodes_to <- node_coords[[i + 1]]
      for (k in 1:nrow(nodes_from)) {
        for (j in 1:nrow(nodes_to)) {
          segments(x0 = nodes_from$x[k], y0 = nodes_from$y[k],
                   x1 = nodes_to$x[j], y1 = nodes_to$y[j])
        }
      }
    }
  }
} else {
  stop("Only a list of number maybe drawn as a neural network. Perhaps apply lay() first.")
}

# Example usage:
layers <- c(3, 5, 2)  # Define the number of nodes in each layer
plot_neural_net(layers)





