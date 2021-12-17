
# TODO write these in terms of defines, foreach over end dates
${OUTDIR}/sgtf/2021-12-06:
	mkdir -p $@

${OUTDIR}/sgtf/2021-11-27:
	mkdir -p $@

${OUTDIR}/sgtf/2021-12-06/%.rdata: R/%_fitting.R ${INDIR}/sgtf.rds R/bbmle_utils.R | ${OUTDIR}/sgtf/2021-12-06
	$(call R)

${OUTDIR}/sgtf/2021-11-27/%.rdata: R/%_fitting.R ${INDIR}/sgtf.rds R/bbmle_utils.R | ${OUTDIR}/sgtf/2021-11-27
	$(call R)

${OUTDIR}/sgtf/%/mergedfit.rds: R/sgtf_fit_consolidate.R | ${OUTDIR}/sgtf/%
	$(call R, $|)

${OUTDIR}/sgtf/%/sims.rds: R/sgtf_ensemble.R ${OUTDIR}/sgtf/%/mergedfit.rds
	$(call R)

.PRECIOUS: ${OUTDIR}/sgtf/%.rds

SGTFMODELS := 00_ssbetabin 01_ssbin 02_betabin

sgtffitting: $(patsubst %,${OUTDIR}/sgtf/2021-12-06/%.rdata,${SGTFMODELS}) \
$(patsubst %,${OUTDIR}/sgtf/2021-11-27/%.rdata,${SGTFMODELS})

sgtfensembling: $(patsubst %,${OUTDIR}/sgtf/2021-%/sims.rds,12-06 11-27)
