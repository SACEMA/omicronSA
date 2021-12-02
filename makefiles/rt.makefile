
${OUTDIR}/omicron_ratios: R/est_rt_ratios.R ${INS}
	$(call R)

${OUTDIR}/omicron_ratios.rds: R/consolidate.R ${OUTDIR}/omicron_ratios
	$(call R)

${FIGDIR}/omicron_ratios.png: R/rt_ratios.R ${OUTDIR}/omicron_ratios.rds
	$(call R)
