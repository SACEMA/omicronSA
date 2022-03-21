
REDFAC := 0.5

#' inputs needed for creating Rt inputs
ENSIN := $(addprefix ${INDIR}/,incidence.rds tmb.rda)

incerr = ${DATADIR}/incens_$(1).log

#' create incidence ensembles, based on end dates from reference ensemble
${OUTDIR}/%/incidence_ensemble.rds: R/rt/ensemble.R ${ENSIN} ${OUTDIR}/%/ensemble.rds
	$(call R,$*) 2> $(call incerr,$*)

${OUTDIR}/%/ratios.rds: R/rt/consolidate.R $(wildcard ${ESTDIR}/%/*_${RTPAT}) | ${ESTDIR}/%
	Rscript $$< $$| ${RTPAT} $$@


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

RTPAT := rt.rds

rterr = ${DATADIR}/rt_$(1).log

${DATADIR}/rt_%.rds: R/rt/logexport.R ${DATADIR}/rt_%.log
	$(call R)

${DATADIR}/rt_check.rds: R/rt/logreview.R | rtlogs
	$(call R,${DATADIR})

cleanrterr:
	rm -i ${DATADIR}/rt_*.log

# n.b. rule for ${OUTDIR}/${DATE} defined in fitting.makefile
define jsondep =
${OUTDIR}/$(1)/$(2): | ${OUTDIR}/$(1)
	mkdir -p $$@

rtdefaults: ${OUTDIR}/$(1)/$(2)

# the % match here is province_sample. so we're making
# ${OUTDIR}/${DATE}/${SCENARIO}/${PROV}_${SAMPLE}_rt.rds
# note: for documenting errors, we are appending to the error log,
# so it needs to be manually reset with `cleanrterr` target
${OUTDIR}/$(1)/$(2)/%_${RTPAT}: R/rt/estimate.R ${OUTDIR}/$(1)/incidence_ensemble.rds ${INDIR}/$(2).json | ${OUTDIR}/$(1)/$(2)
	echo $$* >> $(call rterr,$(1)_$(2))
	$$(call R,$$(subst _, ,$$*)) 2>> $(call rterr,$(1)_$(2))

rtlogs: $(patsubst %.log,%.rds,$(call rterr,$(1)_$(2)))

endef

#' $1 is a YYYY-MM-DD date
define rtdates =
$(eval $(foreach scn,${VARSCNS},$(call jsondep,$(1),${scn})))

#FIXME: currently runs indefinitely for 11-27 truncation date
rtdefaults: ${OUTDIR}/$(1)/incidence_ensemble.rds

endef

$(eval $(foreach date,${CENSORDATES},$(call rtdates,${date})))

RTSAMPS ?= 50

${INDIR}/rtslurmref.txt: R/rt/slurm.R ${INDIR}/sgtf.rds | \
	$(foreach date,${CENSORDATES},$(addprefix ${OUTDIR}/${date}/,${VARSCNS}))
	$(call R,$| ${RTSAMPS})
	wc -l $@
	more $@

rtdefaults: ${INDIR}/rtslurmref.txt

PROVS := $(shell Rscript -e "require(data.table, quietly = TRUE); if (file.exists('${INDIR}/sgtf.rds')) cat(readRDS('${INDIR}/sgtf.rds')[prov != 'EC', unique(prov) ])")

examplert: $(shell cat ${INDIR}/rtslurmref.txt)

consolidatert: $(patsubst %,${OUTDIR}/%/ratios.rds,${CENSORDATES})

