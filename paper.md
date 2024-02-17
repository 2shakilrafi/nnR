---
title: 'nnR: A package to perform algebraic operations on neural networks'
tags:
- R
- neural networks
- albebraic operations
- function composition
- numerical approximations
date: "16 February 2024"
output: pdf_document
authors:
- name: Shakil Rafi
  orcid: "0000-0003-3791-9697"
  equal-contrib: true
  affiliation: 1, 2
- name: Joshua Lee Padgett
  equal-contrib: false
  affiliation: 3
bibliography: paper.bib
aas-doi: "10.3847/xxxxx <- update this with the DOI from AAS once you know it."
aas-journal: "Astrophysical Journal <- The name of the AAS journal."
affiliations:
- name: Department of Mathematical Sciences, University of Arkansas, Fayetteville,
    AR, 72701.
  index: 1
- name: Department of Data Science, University of Arkansas, Fayetteville, AR, 72701.
  index: 2
- name: Toyota Financial Services, Plano, TX, 75024.
  index: 3
---

# Summary

There exists an algebraic way of viewing feedforward multi-layer perceptrons as an ordered tuple of ordered pairs.
That is to say a neural network is $\nu = ((W_1,b_1),(W_2,b_2),...(W_L,b_L))$, for some collection of weight matrices $W_i$ and bias vectors $b_i$.

These were first introduced in @petersen2018, extended in [@grohs2023], and [@bigbook], and along with that several operations such as composition, stacking, scalar left and right multiplication, neural network sums and finally neural networks for squaring and products of real numbers were introduced.

This was extended in [@rafi_towards_2024] to include neural networks for raising to a power, neural network polynomials, neural network exponentials, sines, cosines, and a rudimentary trapezoidal rule implementation.

# Statement of need

Despite this framework's wide use in e.g. @Grohs_2022, @grohsetal, and @ackermann2023deep, in addition to @grohs2019spacetime and @bigbook, this framework has lacked a consistent implementation in a widely available language.

A custom solution is needed as widely available artificial neural network software like PyTorch or TensorFlow are inadequate to the task.
Specifically the authors believe that operations such as the Pwr network with its associated tunnel networks cannot adequately be described using the layer structure that PyTorch or indeed TensorFlow require.
This is because of the way this network is defined, purely in terms of its associated parameters $q$ and $\varepsilon$.
This would lead to an uncountably infinitely many versions of the same network, each needing to be implemented separately.

In addition, this framework only envisions neural networks whose weight and bias parameters are already given.
Typical neural network libraries require training via backpropagation, and it is somewhat difficult to see how one may implement a neural network in these common libraries without actually specifying backpropagation.

Finally, all neural network architectures posited to exist in [@rafi_towards_2024], such as Pwr, Xpn, Csn, Sne, and the 1-D interpolation scheme have associated with theme extensive, and sometimes very convoluted bounds for parameter, depth, and accuracy.
It is nice to have an implementation in R if only to run simulations, but also to test out some of these bounds that are proposed, and further as pedagogical tools to promote a wider dissemination of this rather new neural network calculus.

As a bonus, R's widely known and easy to use infix notation with %function% allows us to translate directly, theorems in @rafi_towards_2024, and @grohs2019spacetime, to software.
For instance $\frac{1}{2}\triangleright(\nu_1 \bullet \nu_2)$ is rendered directly as $0.5 \:\%slm\% \:(nu\_1 \: \%comp\% \: nu\_2)$.

# Mathematics

# Citations

# Figures

# Acknowledgements

# References
