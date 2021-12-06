
ESTDIR := ${OUTDIR}/omicron_ratios

omiratios: ${OUTDIR}/omicron_ratios.rds

RTESTS := $(addprefix ${ESTDIR}/,omicron omicronlow delta)

${OUTDIR}/omicron_ratios.rds: R/consolidate.R ${RTESTS} | ${ESTDIR}
	Rscript $< $| $@

${ESTDIR}:
	mkdir -p $@

.PRECIOUS: ${ESTDIR}/%

${ESTDIR}/%: R/est_rt_ratios.R ${INDIR}/timing.rds ${INDIR}/incidence_ensemble.rds | ${ESTDIR}
	$(call R)
	touch $@

cleanratios:
	rm -rf ${RTESTS}
