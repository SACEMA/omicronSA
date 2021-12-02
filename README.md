# OVERVIEW

This analysis estimates the mixture of transmissibility and immune escape for SARS-CoV-2 Omicron relative to Delta.

# Methods Summary

 1. Estimate the relative frequency of Omicron and Delta from S-Gene Target Failure (SGTF) and create a sample ensemble
 2. Using sample ensemble and incidence data, including reinfections, create incidence ensemble
 3. Estimate Rt for omicron and delta from ensembles, including Rt for omicron assuming shorter generation interval
 4. Calculate ensemble of ratios
 5. Compute NGM principle eigenvalue ratio for Omicron / Delta, assuming varying levels of immune escape for Omicron, including ratio for a shorter Omicron GI
 6. Calculate transmissibility multiplier ensemble (= estimated ratio / NGM ratio)

# Repository structure

## Key folders

Folder| Purpose
---|---
[`.devcontainer`](.devcontainer/) | Resources for reproducibility using `vscode` and `docker`.
## Key files

File | Purpose
---|---

# Reproducibillity

## Dependencies

All `R` package dependencies can be installed using (in the working directory of the repository): 

```r
devtools::install_dev_deps(dependencies = TRUE)
```

Alternatively the dependencies can be installled manually (see the `DESCRIPTION` file for details of the dependencies and their sources). Finally, the provided `Dockerfile` may be used to fully reproduce our analysis environment. See the [Docker documentation](https://docs.docker.com) for more detail on using docker. For `vscode` users a `.devcontainer` has been provided which when used with the `Remote development` extension will automate setting up the supplied `Dockerfile`

## Data requirements
