
${OUTDIR}/ngm_ratios.rds: R/ngm_ratio.R ${DATADIR}/contact_matrices.rds ${DATADIR}/covidm_fit_yu.qs \
${INDIR}/susceptibility.rds ${INDIR}/timing.rds ${MOB}
	$(call R)