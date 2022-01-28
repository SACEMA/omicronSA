
# these definitions used globally; refers to general days-between-tests
# reinfection limit definition (THRSHLDS) and SGTF exclusion/interpretation
# approach (ALTSHLDS)

THRSHLDS := 90 60 30
ALTSHLDS := trim hold
BASETHR := $(firstword ${THRSHLDS})
REFTYPE ?= $(firstword ${ALTSHLDS})
NCEMOUT := Rescapable Vescapable non-reinfectable

# we have made the data necessary to perform the associated analyses available
# via the repository; the raw elements (i.e. line lists) are potentially
# available with a formal request. if doing that, this section transfers files
# from DLDIR (assumed ~/Downloads, but adjustable in local.makefile) to refdata.

INFILES := $(addsuffix .RDS,${NCEMOUT}) pos_test_ll_%.RDS prov_ts_%_pub.RDS

$(eval $(foreach cpfile,${INFILES},$(call cptar,$(cpfile))))

${DATADIR}/sgtf_list_anon%.dta: $(wildcard ${DLDIR}/sgtf_list_anon*.dta)
	cp ${DLDIR}/sgtf_list_anon*.dta ${DATADIR}/.

# set the reference source data for SGTF analysis; defaults to most recently
# updated file matching the below pattern in the downloads directory
RAWSGTF ?= ${DATADIR}/$(shell cd ${DLDIR}; ls -t sgtf_list_anon_*.dta | head -1)

inputdefaults: ${RAWSGTF} \
	$(patsubst %,${DATADIR}/pos_test_ll_%.RDS,${THRSHLDS}) \
	$(patsubst %,${DATADIR}/prov_ts_%_pub.RDS,${THRSHLDS}) \
	$(patsubst %,${DATADIR}/%.RDS,${NCEMOUT})

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

${DATADIR}/sgtf_ll_%.rds: R/sgtf/link_%.R ${DATADIR}/sgtf_ll_${BASETHR}.rds
	$(call R)

.PRECIOUS: ${DATADIR}/sgtf_ll_%.rds

${DATADIR}/sgtf_%.csv: R/sgtf/public.R ${DATADIR}/sgtf_ll_%.rds
	$(call R)

${INDIR}/sgtf_%.rds: R/sgtf/prepare.R ${INDIR}/tmb.rda | ${DATADIR}/sgtf_%.csv
	$(call R,$|)

REFSGTF := ${INDIR}/sgtf_${REFTYPE}.rds

$(eval $(call lngen,${INDIR}/sgtf.rds,${REFSGTF}))

inputdefaults: ${INDIR}/sgtf.rds $(patsubst %,${DATADIR}/sgtf_%.csv,${THRSHLDS} ${ALTSHLDS}) \
	$(patsubst %,${INDIR}/sgtf_%.rds,${THRSHLDS} ${ALTSHLDS})

# end SGTF setup steps #########################################################

# this section deals with generating reference incidence time series

# TODO define target to fetch reinfections series from preprint data sharing
${INDIR}/incidence_%.rds: R/incidence.R ${DATADIR}/prov_ts_%_pub.RDS
	$(call R)

# TODO more sophisticated approach? very limited number of records affected
${INDIR}/incidence_trim.rds ${INDIR}/incidence_hold.rds: ${INDIR}/incidence_${BASETHR}.rds
	$(call ln,$<,$@)

REFINC ?= ${INDIR}/incidence_${REFTYPE}.rds

$(eval $(call lngen,${INDIR}/incidence.rds,${REFINC}))

inputdefaults: ${INDIR}/incidence.rds \
	$(patsubst %,${INDIR}/incidence_%.rds,${THRSHLDS} ${ALTSHLDS})

# end incidence section ########################################################


# other inputs
${INDIR}/susceptibility.rds: R/susceptibility.R $(patsubst %,${DATADIR}/%.RDS,${NCEMOUT})
	$(call R)

# define focal periods for estimation, by province
${INDIR}/timing.rds: R/timing.R | ${INDIR}
	$(call R)

# timing references for various calculations
${INDIR}/tmb.rda: R/fitting/tmb_funs.R | ${INDIR}
	$(call R)

${INDIR}/%/incidence_ensemble.rds: R/ensemble.R ${INDIR}/incidence.rds ${OUTDIR}/sgtf/%/sims.rds
	$(call R)

inputdefaults: ${INDIR}/susceptibility.rds ${INDIR}/timing.rds ${INDIR}/tmb.rda

inputclean:
	rm -f ${DATADIR}/pos_test_ll_*.RDS
	rm -f ${DATADIR}/prov_ts_*.RDS
	rm -f ${DATADIR}/*.dta
	rm -f ${DATADIR}/sgtf_list_anon.rds
	rm -f $(patsubst %,${DATADIR}/%.RDS,${NCEMOUT})
	rm -f ${DATADIR}/*.log
	rm -f ${DATADIR}/sgtf_ll_*.rds
	rm -f ${INDIR}/sgtf_*.rds
	rm -f ${INDIR}/sgtf.rds
	rm -f ${INDIR}/susceptibility.rds
	rm -f ${INDIR}/incidence_*.rds
	rm -f ${INDIR}/incidence.rds
