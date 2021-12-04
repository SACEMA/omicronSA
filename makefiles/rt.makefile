
ESTDIR := ${OUTDIR}/omicron_ratios

omiratios: ${OUTDIR}/omicron_ratios.rds

${OUTDIR}/omicron_ratios.rds: R/consolidate.R ${ESTDIR}/omicron ${ESTDIR}/omicronlow ${ESTDIR}/delta
	$(call R)

${ESTDIR}:
	mkdir -p $@

.PRECIOUS: ${ESTDIR}/%

${ESTDIR}/%: R/est_rt_ratios.R ${INDIR}/timing.rds ${INDIR}/incidence_ensemble.rds | ${ESTDIR}
	$(call R)

${FIGDIR}/omicron_ratios.png: R/rt_ratios.R ${OUTDIR}/omicron_ratios.rds
	$(call R)
