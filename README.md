# Estimating Treatment Effects on Proportions with Synthetic Controls

This is the github rep for the paper "Estimating Treatment Effects on Proportions with Synthetic Controls". 

## Package

The subfolder pkg/ includes the popsdid package. 

It contains extended code from synthdid package to include options to estimate synthetic control methods with common weights  https://synth-inference.github.io/synthdid/ 

It can be installed:

> devtools::install_github("lstoetze/propsdid", subdir="pkg")

## Application

The subfolder app/ includes applications to replication papers

1. app/bolet/ includes code to replicate our application to the empireical analysis in  
     Bolet et al. "How to Get Coal Country to Vote for Climate Policy: The Effect of a Just Transition Agreement on Spanish Election Results" https://doi.org/10.1017/S0003055423001235

2. app/poland/ includes code to replicate our application to the empireical analysis in  
     Haas et al. "The Electoral Effects of State-Sponsored Anti-LGBTQ Measures" https://doi.org/10.1086/739782

## Simulation

The subfolder sim/ includes code for the simulation code of the paper
