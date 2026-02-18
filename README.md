# Estimating Treatment Effects on Proportions with Synthetic Controls

This repository accompanies the paper **“Estimating Treatment Effects on Proportions with Synthetic Controls.”**

---

## Package

The subfolder **`pkg/`** contains the **`propsdid`** package.  
This package extends the functionality of the [**synthdid**](https://synth-inference.github.io/synthdid/) package to allow estimation of Synthetic Difference‑in‑Differences and Synthetic Control models **with common weights across outcomes**.

To install the package directly from GitHub, run:

`devtools::install_github("lstoetze/propsdid", subdir = "pkg")`


---

## Applications

The subfolder **`app/`** includes replication applications for two published studies:

1. **`app/bolet/`** – replication of our application to the empirical analysis in  
   Bolet et al., *“How to Get Coal Country to Vote for Climate Policy: The Effect of a Just Transition Agreement on Spanish Election Results.”*  
   [https://doi.org/10.1017/S0003055423001235](https://doi.org/10.1017/S0003055423001235)

2. **`app/poland/`** – replication of our application to the empirical analysis in  
   Haas et al., *“The Electoral Effects of State‑Sponsored Anti‑LGBTQ Measures.”*  
   [https://doi.org/10.1086/739782](https://doi.org/10.1086/739782)

---

## Simulation

The subfolder **`sim/`** contains the simulation code used in the paper.
