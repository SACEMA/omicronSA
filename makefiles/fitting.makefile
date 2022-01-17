
CENSORDATES := $(addprefix 2021-,12-06 11-27)

# support functions for using TMB
TMBSO := C/logistic.so
TMBRD := ${INDIR}/tmb.rda

${TMBSO}: C/logistic.cpp C/logistic_fit.h
	Rscript -e "TMB::compile('$<')"

${TMBRD}: R/fitting/tmb_funs.R

TMBSHR := ${TMBSO} ${TMBRD}

fittingdefaults: ${TMBSHR}

# END support functions for TMB ###########################

# TODO remove this intermediate step in favor of calculating these
# from outset
# reformatting sgtf inputs for fitting

${OUTDIR}/sgtf_%.rds: R/fitting/reagg.R ${INDIR}/sgtf_%.rds
	$(call R)

${OUTDIR}/sgtf.rds: R/fitting/reagg.R ${INDIR}/sgtf.rds
	$(call R)

fittingdefaults: ${OUTDIR}/sgtf.rds \
	$(subst ${INDIR},${OUTDIR},$(wildcard ${INDIR}/sgtf_*.rds))

# END sgtf reformatting ########################################

define fitdates =
${OUTDIR}/sgtffit/$(1): | ${OUTDIR}/sgtffit
	mkdir -p $$@

${OUTDIR}/sgtffit/$(1)/reference.rdata: ${OUTDIR}/sgtffit/$(1)/${REFTYPE}.rdata
	$$(call ln,$$<,$$@)

endef

define fitviews =
${OUTDIR}/sgtffit/$(1)/$(2).rdata: R/fitting/TBD.R ${INDIR}/sgtf_$(2).rds \
${TMBSO} | ${OUTDIR}/sgtffit/$(1)
	$$(call R)

endef

$(eval $(foreach enddate,${CENSORDATES},$(call fitdates,${enddate})))
$(eval $(foreach enddate,${CENSORDATES},$(foreach view,${THRSHLDS} ${ALTSHLDS},$(call fitviews,${enddate},${view}))))

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
