
ESTDIR := ${OUTDIR}/omicron_ratios

omiratios: ${OUTDIR}/omicron_ratios.rds

RTESTS := $(addprefix ${ESTDIR}/,omicron omicronlow delta)

${OUTDIR}/omicron_ratios.rds: R/consolidate.R ${RTESTS}
	$(call R)

${ESTDIR}:
	mkdir -p $@

.PRECIOUS: ${ESTDIR}/%

${ESTDIR}/%: R/est_rt_ratios.R ${INDIR}/timing.rds ${INDIR}/incidence_ensemble.rds | ${ESTDIR}
	$(call R)
	touch $@

${FIGDIR}/omicron_ratios.png: R/rt_ratios.R ${OUTDIR}/omicron_ratios.rds
	$(call R)

cleanratios:
	rm -rf ${RTESTS}
