
# TODO write these in terms of defines, foreach over end dates
${OUTDIR}/sgtf/2021-12-06 ${OUTDIR}/sgtf/2021-11-27:
	mkdir -p $@

${OUTDIR}/sgtf/2021-12-06/%.rds: R/%_fitting.R ${INDIR}/sgtf.rds R/bbmle_utils.R | ${OUTDIR}/sgtf/2021-12-06
	$(call R)

${OUTDIR}/sgtf/2021-11-27/%.rds: R/%_fitting.R ${INDIR}/sgtf.rds R/bbmle_utils.R | ${OUTDIR}/sgtf/2021-11-27
	$(call R)

${OUTDIR}/sgtf/2021-12-06/sims.%.rds: R/sgtf_ensemble.R ${OUTDIR}/sgtf/2021-12-06/%.rds | ${OUTDIR}/sgtf/2021-12-06
	$(call R)

${OUTDIR}/sgtf/2021-11-27/sims.%.rds: R/sgtf_ensemble.R ${OUTDIR}/sgtf/2021-11-27/%.rds | ${OUTDIR}/sgtf/2021-11-27
	$(call R)

.PRECIOUS: ${OUTDIR}/sgtf/%.rds

SGTFMODELS := ssbin ssbetabin

sgtffitting: $(patsubst %,${OUTDIR}/sgtf/2021-12-06/%.rds,${SGTFMODELS}) \
$(patsubst %,${OUTDIR}/sgtf/2021-11-27/%.rds,${SGTFMODELS})

sgtfensembling: $(patsubst %,${OUTDIR}/sgtf/2021-12-06/sims.%.rds,${SGTFMODELS}) \
$(patsubst %,${OUTDIR}/sgtf/2021-11-27/sims.%.rds,${SGTFMODELS})
