
ESTDIR := ${OUTDIR}/omicron_ratios
OTHERRTDIR := ${OUTDIR}/rt

omiratios:
	${MAKE} ${OUTDIR}/2021-12-06/omicron_ratios.rds TARDATE=2021-12-06
	${MAKE} ${OUTDIR}/2021-11-27/omicron_ratios.rds TARDATE=2021-11-27

rtstudy: ${OTHERRTDIR}

RTESTS := $(addprefix ${ESTDIR}/,omicron omicronlow delta)

${OUTDIR}/%/omicron_ratios.rds: R/consolidate.R ${RTESTS} | ${ESTDIR}
	mkdir -p $(@D)
	Rscript $< $| $* $@

${ESTDIR}:
	mkdir -p $@

.PRECIOUS: ${ESTDIR}/%

${OTHERRTDIR}: R/rt_primary_vs_reinf.R ${INDIR}/incidence.rds
	$(call R)
	touch $@

TARDATE ?= 2021-12-06
TARPROV ?= GP
TARSAMP ?= 01

${INDIR}/delta.json ${INDIR}/omicron.json: R/baseline_GI.R | ${INDIR}
	$(call R)

${ESTDIR}/%/${TARPROV}_${TARSAMP}/${TARDATE}/estimate_samples.rds: R/est_rt_ratios.R ${OUTDIR}/${TARDATE}/incidence_ensemble.rds ${INDIR}/%.json | ${ESTDIR}
	$(call R,${TARPROV} ${TARSAMP})
	touch $(@D)

cleanratios:
	rm -rf ${RTESTS}
