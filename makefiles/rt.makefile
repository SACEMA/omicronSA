
${OUTPUT}/omicron_ratios: R/est_rt_ratios.R ${INS}
	$(call R)

${OUTPUT}/omicron_ratios.rds: R/consolidate.R ${OUTPUT}/omicron_ratios
	$(call R)

${FIGDIR}/omicron_ratios.png: R/rt_ratios.R ${OUTPUT}/omicron_ratios.rds
	$(call R)
