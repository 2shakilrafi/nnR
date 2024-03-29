% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Nrm.R
\name{Nrm}
\alias{Nrm}
\title{Nrm}
\usage{
Nrm(d)
}
\arguments{
\item{d}{the dimensions of the vector or list being normed.}
}
\value{
a neural network that takes the 1-norm of a vector of
size d.under ReLU activation.

\emph{Note:} This function is split into two cases
much like the definition itself.

\emph{Note:} If you choose to specify a \eqn{d} other that \eqn{0} you must instantiate with
a vector or list of that length.

For a specific definition, see:
}
\description{
A function that creates the \eqn{\mathsf{Nrm}} neural networks.that take
the 1- norm of a \eqn{d}-dimensional vector when instantiated with ReLU
activation.
}
\examples{
Nrm(2) |> inst(ReLU, c(5,6))
Nrm(5) |> inst(ReLU,c(0,-9,3,4,-11))


}
\references{
Lemma 4.2.1. Jentzen, A., Kuckuck, B., and von Wurstemberger, P. (2023).
Mathematical introduction to deep learning: Methods, implementations,
and theory. \url{https://arxiv.org/abs/2310.20360}
}
