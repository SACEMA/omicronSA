
CENSORDATES := $(addprefix 2021-,12-06 11-27)

# support functions for using TMB
TMBSO := C/logistic.so
# note: target definition in inputs.makefile
TMBRD := ${INDIR}/tmb.rda

${TMBSO}: C/logistic.cpp C/logistic_fit.h
	Rscript -e "TMB::compile('$<')"

TMBSHR := ${TMBRD} ${TMBSO}

fittingdefaults: ${TMBSHR}

# END support functions for TMB ###########################

# TODO remove this intermediate step in favor of calculating these
# from outset?
# reformatting sgtf inputs for fitting

# END sgtf reformatting ########################################

define fitdates =
${OUTDIR}/$(1): | ${OUTDIR}
	mkdir -p $$@

${OUTDIR}/$(1)/fit.rds: R/fitting/fit.R ${INDIR}/sgtf.rds ${TMBSHR} | ${OUTDIR}/$(1)
	$$(call R)

${OUTDIR}/$(1)/ensemble.rds: R/fitting/ensemble.R ${TMBRD} ${OUTDIR}/$(1)/fit.rds | ${OUTDIR}/$(1)
	$$(call R)

${OUTDIR}/$(1)/fit_%.rds: R/fitting/fit.R ${INDIR}/sgtf_%.rds ${TMBSHR} | ${OUTDIR}/$(1)
	$$(call R)

${OUTDIR}/$(1)/ensemble_%.rds: R/fitting/ensemble.R ${TMBRD} ${OUTDIR}/$(1)/fit_%.rds | ${OUTDIR}/$(1)
	$$(call R)

.PRECIOUS: ${OUTDIR}/$(1)/fit_%.rds ${OUTDIR}/$(1)/fit.rds

fittingdefaults: ${OUTDIR}/$(1)/ensemble.rds $(patsubst %,${OUTDIR}/$(1)/ensemble_%.rds,${THRSHLDS} ${ALTSHLDS})

endef

$(eval $(foreach enddate,${CENSORDATES},$(call fitdates,${enddate})))
$(eval $(foreach enddate,${CENSORDATES},$(foreach view,${THRSHLDS} ${ALTSHLDS},$(call fitviews,${enddate},${view}))))

fittingclean:
	rm -f ${OUTDIR}/*/fit_*.rds ${OUTDIR}/*/fit.rds
	rm -f ${OUTDIR}/*/ensemble_*.rds ${OUTDIR}/*/ensemble.rds
