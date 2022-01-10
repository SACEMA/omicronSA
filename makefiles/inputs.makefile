
# we have made the data necessary to perform the associated analyses available
# via the repository; the raw elements (i.e. line lists) are potentially
# available with a formal request. if doing that, this section transfers files
# from DLDIR (assumed ~/Downloads, but adjustable in local.makefile) to refdata.

INFILES := Rescapable.RDS Vescapable.RDS non-reinfectable.RDS \
pos_test_ll_%.RDS prov_ts_%.RDS

$(eval $(foreach cpfile,${INFILES},$(call cptar,$(cpfile))))

${DATADIR}/sgtf_list_anon%.dta: $(wildcard ${DLDIR}/sgtf_list_anon*.dta)
	cp ${DLDIR}/sgtf_list_anon*.dta ${DATADIR}/.

# set the reference source data for SGTF analysis; defaults to most recently
# updated file matching the below pattern in the downloads directory
RAWSGTF ?= ${DATADIR}/$(shell cd ${DLDIR}; ls -t sgtf_list_anon_*.dta | head -1)

inputdefaults: ${RAWSGTF} \
	$(patsubst %,${DATADIR}/pos_test_ll_%.RDS,30 60 90) \
	$(patsubst %,${DATADIR}/prov_ts_%.RDS,90) \
	$(patsubst %,${DATADIR}/%.RDS,Rescapable Vescapable non-reinfectable)

# end data transfer section ####################################################

# SGTF data is a key input to the analysis. Working from the SGTF and positive
# test line lists (raw data not generally made available), we develop SGTF
# time series (included in this repository as csvs)

# define logging for warnings emitted by sgtf processing
sgtferr = ${DATADIR}/sgtf_$(1).log

${DATADIR}/sgtf_list_anon.rds: R/sgtf/import.R ${RAWSGTF}
	$(call R) 2> $(call sgtferr,import)

${DATADIR}/sgtf_ll_%.rds: R/sgtf/link.R ${DATADIR}/sgtf_list_anon.rds ${DATADIR}/pos_test_ll_%.RDS
	$(call R)

${DATADIR}/sgtf_ll_%.rds: R/sgtf/link_%.R ${DATADIR}/sgtf_ll_90.rds
	$(call R)

.PRECIOUS: ${DATADIR}/sgtf_ll_%.rds

${DATADIR}/sgtf_%.csv: R/sgtf/public.R ${DATADIR}/sgtf_ll_%.rds
	$(call R)

${INDIR}/sgtf_%.rds: R/sgtf.R | ${DATADIR}/sgtf_%.csv
	$(call R,$|)

REFTYPE ?= trim

REFSGTF ?= ${INDIR}/sgtf_${REFTYPE}.rds

$(eval $(call cpgen,${INDIR}/sgtf.rds,${REFSGTF}))

inputdefaults: ${INDIR}/sgtf.rds $(patsubst %,${DATADIR}/sgtf_%.csv,30 60 90 hold trim) \
	$(patsubst %,${INDIR}/sgtf_%.rds,30 60 90 hold trim)

# end SGTF setup steps #########################################################

# this section deals with generating reference incidence time series

# TODO define target to fetch reinfections series from preprint data sharing
${INDIR}/incidence_%.rds: R/incidence.R ${DATADIR}/prov_ts_%.RDS
	$(call R)

# TODO more sophisticated approach? very limited number of records affected
${INDIR}/incidence_trim.rds ${INDIR}/incidence_hold.rds: ${INDIR}/incidence_90.rds
	cp $< $@

REFINC ?= ${INDIR}/incidence_${REFTYPE}.rds

$(eval $(call cpgen,${INDIR}/incidence.rds,${REFINC}))

inputdefaults: ${INDIR}/incidence.rds \
	$(patsubst %,${INDIR}/incidence_%.rds,90 hold trim)

# end incidence section ########################################################


# other inputs
${INDIR}/susceptibility.rds: R/susceptibility.R $(patsubst %,${DATADIR}/%.RDS,Rescapable Vescapable non-reinfectable)
	$(call R)

# define focal periods for estimation, by province
${INDIR}/timing.rds: R/timing.R | ${INDIR}
	$(call R)

${INDIR}/%/incidence_ensemble.rds: R/ensemble.R ${INDIR}/incidence.rds ${OUTDIR}/sgtf/%/sims.rds
	$(call R)

inputdefaults: ${INDIR}/susceptibility.rds ${INDIR}/timing.rds











