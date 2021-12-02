
# for non-make users:
# general syntax is
# 
# target: dependency1 dependency2 ...
#   commands run in the terminal
# 
# target & dependencies being files
# in general, this repo follows a paradigm of listing code file as first dependency,
# followed by inputs files to that code

# reference definitions; optionally create a local.makefile to overide
# REFDIR & FIGFMT; can also specify in invocation, e.g. `make sometarget FIGFMT=jpg`
-include makefiles/local.makefile

include makefiles/paths.makefile

default: all
all: support inputs rt mobillity analyses

# automates some setup tasks & provides convenience definitions
# particular, provides definition of R such that:
# $(call R) = Rscript [dependencies] [target]
# $(call R,${THING}) = Rscript [dependencies] ${THING} [target]
include makefiles/support.makefile
support: dirs

# details for formatting input data
include makefiles/inputs.makefile
inputs: ${INS} ${INDIR}/susceptibility.rds

# details for estimating Rt, ratios figures etc
include makefiles/rt.makefile
rt: ${OUTDIR}/omicron_ratios ${OUTDIR}/omicron_ratios.rds ${FIGDIR}/omicron_ratios.png

# details for getting contact matrix adjustments
include makefiles/mobility.makefile
mobility: ${MOB}

# details for analyses using the estimates derived in previous steps
include makefiles/analyses.makefile
analyses: ${INDIR}/susceptibility.rds ${OUTDIR}/ngm_ratios.rds ${OUTDIR}/thresholds.rds ${FIGDIR}/thresholds.png

# get a list of all available targets
.PHONY: list
list:
	grep "^[^#[:space:]].*:" Makefile
	