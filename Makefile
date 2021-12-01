
# reference definitions; optionally create a local.makefile to overide
# REFDIR & FIGFMT; can also specify in invocation, e.g. `make sometarget FIGFMT=jpg`

-include local.makefile

REFDIR ?= PLACEHOLDER
FIGFMT ?= png
LNDIR := analysis
INDIR  := ${LNDIR}/input
OUTDIR := ${LNDIR}/output
FIGDIR := ${OUTDIR}/fig
MKDIRS := ${OUTDIR} ${INDIR}

VPATH = ${INDIR}:${OUTDIR}

# automates some setup tasks & provides convenience definitions
include support.makefile

# define focal periods for estimation, by province
${INDIR}/timing.rds: timing.R | ${INDIR}
	$(call R)

# TODO define target to fetch reinfections series from preprint data sharing
${INDIR}/incidence.rds: incidence.R ${INDIR}/prov_ts_90_pub.RDS
	$(call R)

# JD TODO: fill in rule
# ${INDIR}/frequencies.rds: frequency.R ${INDIR}/SGTF.csv
#	$(call R)

inputs: $(patsubst %,${INDIR}/%.rds,timing incidence frequencies)