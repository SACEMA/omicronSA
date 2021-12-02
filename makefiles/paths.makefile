
REFDIR ?= refdata
FIGFMT ?= png
LNDIR := analysis
INDIR  := ${LNDIR}/input
OUTDIR := ${LNDIR}/output
FIGDIR := ${OUTDIR}/fig
DATADIR := refdata
MKDIRS := ${OUTDIR} ${INDIR}
COVIDM := covidm

VPATH = ${INDIR}:${OUTDIR}
