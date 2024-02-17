---
title: 'nnR: A package to perform algebraic operations on neural networks'
tags:
  - R
  - neural networks
  - albebraic operations
  - function composition
  - numerical approximations
authors:
  - name: Shakil Rafi
    orcid: 0000-0003-3791-9697
    equal-contrib: true
    affiliation: "1, 2" # (Multiple affiliations must be quoted)
  - name: Joshua Lee Padgett
    equal-contrib: false # (This is how you can denote equal contributions between multiple authors)
    affiliation: 3
affiliations:
 - name: Department of Mathematical Sciences, University of Arkansas, Fayetteville, AR, 72701.
   index: 1
 - name: Department of Data Science, University of Arkansas, Fayetteville, AR, 72701.
   index: 2
 - name: Toyota Financial Services, Plano, TX, 75024.
   index: 3
date: 16 February 2024
bibliography: references.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

# Summary

There exists an algebraic way of viewing feedforward multi-layer perceptrons as an ordered tuple of ordered pairs.
That is to say a neural network is $\nu = ((W_1,b_1),(W_2,b_2),...(W_L,b_L))$, for some collection of weight matrices $W_i$ and bias vectors $b_i$.

These were first introduced in [@petersen2018], extended in [@grohs2023], and [@bigbook], and along with that several operations such as composition, stacking, scalar left and right multiplication, neural network sums and finally neural networks for squaring and products of real numbers were introduced.

This was extended in [@rafi_towards_2024] to include neural networks for raising to a power, neural network polynomials, neural network exponentials, sines, cosines, and a rudimentary trapezoidal rule implementation.

# Statement of need

Despite

# Mathematics

# Citations

# Figures

# Acknowledgements

# References
