# Makefiles

This folder contains individual workflow steps as `makefile`s. See the overall `Makefile` for orchestration of these components.

`makefile` | Purpose
---|---
`support.makefile` | Defines helper tools and constructs directories required for storing results.
`inputs.makefile` | Defines helper tools and constructs directories required for storing results.
`rt.makefile` | Estimates the reproduction number for each variant of interest, combines them into a single object, and then calculates the ratios between them and Omicron.
`mobility.makefile` | Downloads and formats Google mobility data, and data from the Oxford COVID-19 government response tracker.
`susceptible.makefile` | Estimate the proportion of the population that is susceptible.
`threshold.makefile` | Use Rt ratios and estimates of susceptible population to calculate thresholds for transmissibility vs immune escape.
`local.makefile` | Not provided by default this `makefile can be used to override settings given in `support.makefile`.