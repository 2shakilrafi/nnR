% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Phi_k.R
\name{C_k}
\alias{C_k}
\title{C_k: The function that returns the C_k matrix}
\usage{
C_k(k)
}
\arguments{
\item{k}{Natural number, the precision with which to approximate squares
within \eqn{[0,1]}}
}
\value{
An intermediate matrix in a
neural network that approximates the square of any real within
\eqn{[0,1]} upon ReLU instantiation.
}
\description{
C_k: The function that returns the C_k matrix
}
\examples{
C_k(5)

}
\references{
Definition 2.22. Rafi S., Padgett, J.L., Nakarmi, U. (2024)
Towards an Algebraic Framework For
Approximating Functions Using Neural Network Polynomials
\url{https://arxiv.org/abs/2402.01058}
}
