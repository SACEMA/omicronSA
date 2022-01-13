
# this intended to be overriden in local.makefile
# HOWEVER, `refdata` (the local directory holding shared data) is a perfectly
# serviceable root directory for running the analysis against
REFDIR ?= refdata
DLDIR ?= ~/Downloads

LNDIR := analysis
INDIR  := ${LNDIR}/input
OUTDIR := ${LNDIR}/output
FIGDIR := ${OUTDIR}/fig
FITDIR := ${OUTDIR}/sgtffit
DATADIR := refdata
MKDIRS := ${OUTDIR} ${INDIR} ${FIGDIR} ${FITDIR}

VPATH = ${INDIR}:${OUTDIR}
