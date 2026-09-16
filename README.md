[![DOI](https://zenodo.org/badge/752787458.svg)](https://zenodo.org/badge/latestdoi/752787458)

<img src="images/hex-nnR-4-01.png" alt="image_alt_text" width="1in" height="1.27in"/>

# nnR

## Neural Networks Made Algebraic

This is a repository which implements a certain neural network calculus.
The implemented calculus derives mainly from [Rafi, Padgett, and Nakarmi (2024)](https://doi.org/10.48550/arXiv.2402.01058), which in turn adapts ideas developed by [Grohs et al. (2023)](https://doi.org/10.1007/s10444-022-09970-2) and [Jentzen, Kuckuck, and von Wurstemberger (2023)](https://doi.org/10.48550/arXiv.2310.20360).

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("2shakilrafi/nnR")
library(nnR)
```

The package requires R 4.1 or newer. Run its tests from a source checkout with `devtools::test()` and build the vignette with `devtools::build_vignettes()`.

## Implemented operations

Our neural network calculus envisions neural networks as an ordered tuple of ordered pairs of $W$ and $b$, weight matrices and bias vectors.

We may compose neural networks as in Definition 2.6 of the primary paper.

We may stack neural networks as in Definition 2.14.

We may take the sum of neural networks as in Definition 2.19.

We may take squares and products of neural networks as in Definitions 2.24 and 2.25, respectively.

We may take powers of neural networks as in Definition 2.26.

We may take neural-network exponentials, sines, and cosines, as in Definitions 2.28, 2.29, and 2.30, respectively.

We may implement the one-dimensional trapezoidal rule for integration as in Definitions 2.31 and 2.33.

Finally, norms, maxima, and a maximum-convolution-type approximation for one-dimensional continuous functions are available as in Definitions 2.35, 2.37, and 2.39, respectively.

See the package documentation and `vignettes/` for function-level guidance. This implementation is primarily a pedagogical proof of concept; benchmark and validate it before performance-sensitive use.
