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
  orcid: "0000-0001-9369-351X"
bibliography: paper.bib
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

These were first introduced in @petersen_optimal_2018 , extended in [@grohsetal], and [@bigbook], and along with that several operations such as composition, stacking, scalar left and right multiplication, neural network sums and finally neural networks for squaring and products of real numbers were introduced.

This was extended in [@rafi_towards_2024] to include neural networks for raising to a power, neural network polynomials, neural network exponentials, sines, cosines, and a rudimentary trapezoidal rule implementation.

This is not the standard way neural networks are envisioned, which are as piecewise linear approximants.
This way of building out networks that approximate transcendental functions, using algebraic operations of sum, scalar multiplication, stacking, products, and raising to powers, differs markedly from deep learning orthodoxy.
It is hoped that this package will find use in the community.

A full explanation of what these algebraic operations are, and what these neural network sines, cosines, and exponents are is explained in [@rafi_towards_2024].

# Statement of need

Despite this framework's wide use in e.g. [@grohsetal], [@Grohs_2022], and [@ackermann2023deep], in addition to [@grohs2019spacetime] and [@bigbook], this framework has lacked a consistent implementation in a widely available language.

A custom solution is needed as widely available artificial neural network software like PyTorch or TensorFlow are inadequate to the task.
Specifically the authors believe that operations such as the Pwr network with its associated tunnel networks cannot adequately be described using the layer structure that PyTorch or indeed TensorFlow require.
This is because of the way this network is defined, purely in terms of its associated parameters $q$ and $\varepsilon$.
This would lead to an uncountably infinitely many versions of the same network, each needing to be implemented separately.

In addition, this framework only envisions neural networks whose weight and bias parameters are already given.
Typical neural network libraries require training via backpropagation, and it is somewhat difficult to see how one may implement a neural network in these common libraries without actually specifying backpropagation.

Finally, all neural network architectures posited to exist in [@rafi_towards_2024], such as Pwr, Xpn, Csn, Sne, and the 1-D interpolation scheme have associated with theme extensive, and sometimes very convoluted bounds for parameter, depth, and accuracy.
It is nice to have an implementation in R if only to run simulations, but also to test out some of these bounds that are proposed, and further as pedagogical tools to promote a wider dissemination of this rather new neural network calculus.

As a bonus, R's widely known and easy to use infix notation with %function% allows us to translate directly, theorems in [@rafi_towards_2024], [@grohs2019spacetime], and [@bigbook] to software.
For instance $\frac{1}{2}\triangleright(\nu_1 \bullet \nu_2)$ is rendered directly as `0.5 %slm% (nu_1  %comp% nu_2)`.

A concerted effort has been made to make this software as fast as possible, with R vectorization.
Future work will be focused on re-implementing this, but using Rcpp.
The source code repository for nnR has been archived to Zenodo with <https://doi.org/10.5281/zenodo.10672209>

# Acknowledgements

This work was conducted fully or in part on computational resources at the Arkansas High Performance Computing Center

# References
