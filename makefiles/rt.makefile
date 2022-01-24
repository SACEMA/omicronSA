
REDFAC := 0.5

${INDIR}/delta.json: R/rt/baseline_GI.R | ${INDIR}
	$(call R)

${INDIR}/omicron.json: ${INDIR}/delta.json
	cp $< $@

${INDIR}/omicronredlat.json: R/rt/reducelat_GI.R
	$(call R,${REDFAC})

${INDIR}/omicronredinf.json: R/rt/reduceinf_GI.R
	$(call R,${REDFAC})

VARSCNS := delta omicron omicronredlat omicronredinf

rtdefaults: $(patsubst %,${INDIR}/%.json,${VARSCNS})

ENSIN := $(addprefix ${INDIR}/,incidence.rds simDates.rda tmb.rda)

RTPAT := rt.rds

define jsondep =
${OUTDIR}/$(1)/$(2):
	mkdir -p $$@

${OUTDIR}/$(1)/$(2)/%_${RTPAT}: R/rt/estimate.R ${OUTDIR}/$(1)/incidence_ensemble.rds ${INDIR}/$(2).json | ${OUTDIR}/$(1)/$(2)
	$$(call R,$$(subst _, ,$$*))

endef

define rtdates =
${OUTDIR}/$(1)/incidence_ensemble.rds: R/rt/ensemble.R ${ENSIN} ${OUTDIR}/$(1)/ensemble.rds
	$$(call R)

$(eval $(foreach scn,${VARSCNS},$(call jsondep,$(1),${scn})))

${OUTDIR}/$(1)/ratios.rds: R/rt/consolidate.R $(wildcard ${ESTDIR}/$(1)/*_${RTPAT}) | ${ESTDIR}/$(1)
	Rscript $$< $$| ${RTPAT} $$@

rtdefaults: ${OUTDIR}/$(1)/incidence_ensemble.rds

endef

$(eval $(foreach date,${CENSORDATES},$(call rtdates,${date})))

#OTHERRTDIR := ${OUTDIR}/rt

#omiratios:
#	${MAKE} ${OUTDIR}/2021-12-06/omicron_ratios.rds TARDATE=2021-12-06
#	${MAKE} ${OUTDIR}/2021-11-27/omicron_ratios.rds TARDATE=2021-11-27

#rtstudy: ${OTHERRTDIR}

#RTESTS := $(addprefix ${ESTDIR}/,omicron omicronlow delta)

#${OUTDIR}/%/omicron_ratios.rds: R/consolidate.R ${RTESTS} | ${ESTDIR}
#	mkdir -p $(@D)
#	Rscript $< $| $* $@

#${ESTDIR}:
#	mkdir -p $@

#.PRECIOUS: ${ESTDIR}/%

#${OTHERRTDIR}: R/rt_primary_vs_reinf.R ${INDIR}/incidence.rds
#	$(call R)
#	touch $@

#${ESTDIR}/%/${TARPROV}_${TARSAMP}/${TARDATE}/estimate_samples.rds: R/est_rt_ratios.R ${OUTDIR}/${TARDATE}/incidence_ensemble.rds ${INDIR}/%.json | ${ESTDIR}
#	$(call R,${TARPROV} ${TARSAMP})
#	touch $(@D)

#cleanratios:
#	rm -rf ${RTESTS}
