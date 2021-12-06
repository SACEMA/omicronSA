
figs: $(patsubst %,${FIGDIR}/%.png,omicron_ratios thresholds)

${FIGDIR}/frequencies.png: R/fig_frequency.R $(addprefix ${INDIR}/,sssims.rds simbig.quasi.rds simbig.bin.rds)
	$(call R)

${FIGDIR}/incidence_ensemble.png: R/fig_incidence_ensemble.R ${OUTDIR}/incidence_ensemble.rds
	$(call R)

${FIGDIR}/omicron_ratios.png: R/fig_rt_ratios.R ${OUTDIR}/omicron_ratios.rds
	$(call R)

${FIGDIR}/thresholds.png: R/fig_thresholds.R ${OUTDIR}/thresholds.rds
	$(call R)
