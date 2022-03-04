
REDFAC := 0.5

#' establish reference generation interval
${INDIR}/delta.json: R/rt/baseline_GI.R | ${INDIR}
	$(call R)

#' scenario: delta == omicron generation interval
${INDIR}/omicron.json: ${INDIR}/delta.json
	cp $< $@

#' scenario: omicron has shorter generation interval, due to shorter
#' latent period (i.e. time from infection => infectiousness)
${INDIR}/omicronredlat.json: R/rt/reducelat_GI.R
	$(call R,${REDFAC})

#' scenario: omicron has shorter generation interval, due to both shorter
#' latent AND infectious period
#' (i.e. time from infection => infectiousness & duration of infectiousness)
${INDIR}/omicronredinf.json: R/rt/reduceinf_GI.R
	$(call R,${REDFAC})

VARSCNS := delta omicron omicronredlat omicronredinf

rtdefaults: $(patsubst %,${INDIR}/%.json,${VARSCNS})

#' inputs needed for creating Rt inputs
ENSIN := $(addprefix ${INDIR}/,incidence.rds tmb.rda)

RTPAT := rt.rds

define jsondep =
${OUTDIR}/$(1)/$(2):
	mkdir -p $$@

${OUTDIR}/$(1)/$(2)/%_${RTPAT}: R/rt/estimate.R ${OUTDIR}/$(1)/incidence_ensemble.rds ${INDIR}/$(2).json | ${OUTDIR}/$(1)/$(2)
	$$(call R,$$(subst _, ,$$*))

endef

incerr = ${DATADIR}/incens_$(1).log

define rtdates =
${OUTDIR}/$(1)/incidence_ensemble.rds: R/rt/ensemble.R ${ENSIN} ${OUTDIR}/$(1)/ensemble.rds
	$$(call R) 2> $$(call incerr,$(1))

$(eval $(foreach scn,${VARSCNS},$(call jsondep,$(1),${scn})))

${OUTDIR}/$(1)/ratios.rds: R/rt/consolidate.R $(wildcard ${ESTDIR}/$(1)/*_${RTPAT}) | ${ESTDIR}/$(1)
	Rscript $$< $$| ${RTPAT} $$@

#FIXME: currently runs indefinitely for 11-27 truncation date
rtdefaults: ${OUTDIR}/$(1)/incidence_ensemble.rds

endef

$(eval $(foreach date,${CENSORDATES},$(call rtdates,${date})))

RTSAMPS ?= 50

${INDIR}/rtslurmref.txt: R/rt/slurm.R ${INDIR}/sgtf.rds | ${OUTDIR} $(addprefix ${OUTDIR}/,)
	$(call R,$(firstword $|) ${RTSAMPS})

rtdefaults: ${INDIR}/rtslurmref.txt

PROVS := $(shell Rscript -e "require(data.table, quietly = TRUE); if (file.exists('${INDIR}/sgtf.rds')) cat(readRDS('${INDIR}/sgtf.rds')[, unique(prov) ])")

examplert: $(foreach pr,${PROVS},$(foreach scn,${VARSCNS},$(patsubst %,${OUTDIR}/2021-12-06/${scn}/${pr}_%_${RTPAT},$(call seq,01,${RTSAMPS}))))

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
