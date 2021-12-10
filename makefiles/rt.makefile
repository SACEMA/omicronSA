
ESTDIR := ${OUTDIR}/omicron_ratios
OTHERRTDIR := ${OUTDIR}/rt

omiratios: ${OUTDIR}/omicron_ratios.rds
rtstudy: ${OTHERRTDIR}

RTESTS := $(addprefix ${ESTDIR}/,omicron omicronlow delta)

${OUTDIR}/omicron_ratios.rds: R/consolidate.R ${RTESTS} | ${ESTDIR}
	Rscript $< $| $@

${ESTDIR}:
	mkdir -p $@

.PRECIOUS: ${ESTDIR}/%

${OTHERRTDIR}: R/rt_primary_vs_reinf.R ${INDIR}/incidence.rds
	$(call R)
	touch $@

${ESTDIR}/%: R/est_rt_ratios.R ${INDIR}/timing.rds ${INDIR}/incidence_ensemble.rds | ${ESTDIR}
	$(call R)
	touch $@

cleanratios:
	rm -rf ${RTESTS}
