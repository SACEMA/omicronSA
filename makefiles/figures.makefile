
RFIG := R/fig
FIGREF := ${INDIR}/plotref.rda

${FIGREF}: ${RFIG}/plotref.R
	$(call R)

figs: $(patsubst %,${FIGDIR}/%.png,2021-12-06/omicron_ratios 2021-11-27/omicron_ratios 2021-12-06/thresholds 2021-11-27/thresholds prime_vs_reinf)

${FIGDIR}/frequencies.png: ${RFIG}/fig_frequency.R $(addprefix ${INDIR}/,sssims.rds simbig.quasi.rds simbig.bin.rds) ${INDIR}/sgtf.rds
		$(call R)

${FIGDIR}/incidence_ensemble.png: ${RFIG}/fig_incidence_ensemble.R ${OUTDIR}/incidence_ensemble.rds
	$(call R)

${FIGDIR}/%/omicron_ratios.png: ${RFIG}/fig_rt_ratios.R ${OUTDIR}/%/omicron_ratios.rds
	$(call R)

${FIGDIR}/prime_vs_reinf.png: ${RFIG}/fig_rt_prime_vs_reinf.R ${OTHERRTDIR}
	$(call R)

${FIGDIR}/%/thresholds.png: ${RFIG}/fig_thresholds.R ${OUTDIR}/%/thresholds.rds
	$(call R)
