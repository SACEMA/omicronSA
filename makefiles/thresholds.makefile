
${OUTDIR}/thresholds.rds: R/thresholds.R ${INDIR}/timing.rds ${OUTDIR}/omicron_ratios.rds ${OUTDIR}/ngm_ratios.rds
	$(call R)

${FIGDIR}/thresholds.png: R/thresholds.R ${OUTDIR}/thresholds.rds
	$(call R)