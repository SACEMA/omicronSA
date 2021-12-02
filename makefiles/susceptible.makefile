
${INDIR}/susceptibility.rds: R/susceptibility.R ${DATADIR}/escapable.rds ${DATADIR}/non_reinfectable.rds
	$(call R)

${OUTDIR}/ngm_ratios.rds: R/ngm_ratio.R ${REFDIR}/contact_matrices.rds ${REFDIR}/covidm_fit_yu.qs \
${INDIR}/susceptibility.rds ${INDIR}/timing.rds ${MOB} | ${COVIDM}
	$(call R,${COVIDM})