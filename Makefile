
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

-include local.makefile

REFDIR ?= PLACEHOLDER
FIGFMT ?= png
LNDIR := analysis
INDIR  := ${LNDIR}/input
OUTDIR := ${LNDIR}/output
FIGDIR := ${OUTDIR}/fig
DATADIR := refdata
MKDIRS := ${OUTDIR} ${INDIR}
COVIDM := ../covidm

VPATH = ${INDIR}:${OUTDIR}

# automates some setup tasks & provides convenience definitions
# particular, provides definition of R such that:
# $(call R) = Rscript [dependencies] [target]
# $(call R,${THING}) = Rscript [dependencies] ${THING} [target]
include support.makefile

# define focal periods for estimation, by province
${INDIR}/timing.rds: timing.R | ${INDIR}
	$(call R)

# TODO define target to fetch reinfections series from preprint data sharing
${INDIR}/incidence.rds: incidence.R ${INDIR}/prov_ts_90_pub.RDS
	$(call R)

# ML/JD TODO: fill in rule
# ${INDIR}/frequencies.rds: frequency.R ${INDIR}/SGTF.csv
#	$(call R)

INS := $(patsubst %,${INDIR}/%.rds,timing incidence frequencies)

inputs: ${INS} ${INDIR}/susceptibility.rds

# TBD TODO:
# ${FIGDIR}/frequency_vis.png: something.R ${INDIR}/frequencies.rds
#	$(call R)
# 
# ${FIGDIR}/var_incidence.png: something2.R ${INS}
#	$(call R)

${OUTPUT}/omicron_ratios: est_rt_ratios.R ${INS}
	$(call R)

${OUTPUT}/omicron_ratios.rds: consolidate.R ${OUTPUT}/omicron_ratios
	$(call R)

${FIGDIR}/omicron_ratios.png: fig/rt_ratios.R ${OUTPUT}/omicron_ratios.rds
	$(call R)

# details for getting contact matrix adjustments
include mobility.makefile

${INDIR}/susceptibility.rds: susceptibility.R ${DATADIR}/escapable.rds ${DATADIR}/non_reinfectable.rds
	$(call R)

${OUTDIR}/ngm_ratios.rds: ngm_ratio.R ${REFDIR}/contact_matrices.rds ${REFDIR}/covidm_fit_yu.qs \
${INDIR}/susceptibility.rds ${INDIR}/timing.rds ${MOB} | ${COVIDM}
	$(call R,${COVIDM})

${OUTDIR}/thresholds.rds: thresholds.R ${INDIR}/timing.rds ${OUTPUT}/omicron_ratios.rds ${OUTDIR}/ngm_ratios.rds
	$(call R)

${FIGDIR}/thresholds.png: fig/thresholds.R ${OUTDIR}/thresholds.rds
	$(call R)