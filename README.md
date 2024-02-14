---
bibliography: references.bib
output:
  html_document:
    df_print: paged
---

This is a repository which implements a certain neural network calculus.
This neural network calculus, or atleast the version implemented derives itself mainly from @rafi2024, which in turn is a highly modified version found in detail in @10.48550/arxiv.2310.20360 and @grohs2023.

Our neural network calculus envisions neural networks as an ordered tuple of ordered pairs of $W$ and $b$, weight matrices and bias vectors.

We may compose neural networks as in Definition 2.6 in @rafi2024.

We may stack neural networks as in Definition 2.14 in @rafi2024.

We may take the sum of neural networks as in Definition 2.19 in @rafi2024.

We may take squares and products of neural networks as in Definition 2.24 and Definition 2.25 of respectively of @rafi2024.
