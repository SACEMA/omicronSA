
${OUTDIR}/sgtf.%.rds: sgtf_fitting.R %.R ${DATADIR}/sgtf.rds
    $(call R)

${OUTDIR}/sims.%.rds: sgtf_ensemble.R %.R sgtf.%.rds
    $(call R)

.PRECIOUS: ${OUTDIR}/sgtf.%.rds

stgffitting: $(patsubst %,${OUTDIR}/sgtf.%.rds,ss basefit)

sgtfensembling: $(patsubst %,${OUTDIR}/sims.%.rds,ss basefit)