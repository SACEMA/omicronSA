
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

${ESTDIR}/%: R/est_rt_ratios.R ${OUTDIR}/${TARDATE}/incidence_ensemble.rds | ${ESTDIR}
	$(call R)
	touch $@

${ESTDIR}/alt/%: R/est_rt_ratios.R ${INDIR}/alt_incidence_ensemble.rds | ${ESTDIR}
	$(call R)
	touch $@

cleanratios:
	rm -rf ${RTESTS}
