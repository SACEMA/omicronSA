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

## Make

This repository uses a `Makefile` to define the analysis workflow. This `Makefile` is split into component `.makefile`'s stored in the `makefiles` folder. To reproduce the analysis (once the dependencies have been installed, and key data sources have been added as outlined below) we can simply run the following in the command line:

```bash
make
```

To see a breakdown of workflow steps run the following instead:

```bash
make list
```

Individual steps can then be updated by running:

```bash
make <target>
```

Individual analysis steps can also be run interactively (see the `R` folder). See the [`makefiles` README](makefiles/) for further details of the component workflow steps.

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
install.packages("devtools")
devtools::install_dev_deps(dependencies = TRUE)
```

Alternatively the dependencies can be installed manually (see the `DESCRIPTION` file for details of the dependencies and their sources). Finally, the provided `Dockerfile` may be used to fully reproduce our analysis environment. See the [Docker documentation](https://docs.docker.com) for more detail on using docker. For `vscode` users a `.devcontainer` has been provided which when used with the `Remote development` extension will automate setting up the supplied `Dockerfile`. The environment last used to generate results is also summarised in `sessioninfo.txt`.

## Data requirements
