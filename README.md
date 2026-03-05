# Estimating Treatment Effects on Proportions with Synthetic Controls

This repository contains the R package `propsdid`, which accompanies the paper Bogatyrev, K., & Stoetzer. _Estimating treatment effects on proportions with synthetic controls_. OSF Preprints. https://osf.io/preprints/osf/brhd3

The package extends the functionality of the [`synthdid` package](https://synth-inference.github.io/synthdid/) by enabling estimation of Synthetic Difference‑in‑Differences and Synthetic Control models with common weights across outcomes.

## Installation

You can install the package directly from GitHub using:

    devtools::install_github("lstoetze/propsdid")

## Description

`propsdid` provides tools for estimating treatment effects on proportions using:

* Synthetic Difference‑in‑Differences with common weights
* Synthetic Control with common weights