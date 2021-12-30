
# define focal periods for estimation, by province
${INDIR}/timing.rds: R/timing.R | ${INDIR}
	$(call R)

# TODO define target to fetch reinfections series from preprint data sharing
${INDIR}/incidence.rds: R/incidence.R ${INDIR}/prov_ts_90.RDS
	$(call R)

${INDIR}/%/incidence_ensemble.rds: R/ensemble.R ${INDIR}/incidence.rds ${OUTDIR}/sgtf/%/sims.rds
	$(call R)

RAWSGTF ?= $(shell ls -t ${DATADIR}/sgtf_list_anon_*.dta | head -1)

SGTFERR = ${DATADIR}/sgtf_$(1).log

${DATADIR}/sgtf_ll_%.rds: R/link_sgtf.R ${DATADIR}/pos_test_ll_%.RDS ${RAWSGTF}
	$(call R) 2> $(call SGTFERR,$*)

.PRECIOUS: ${DATADIR}/sgtf_ll_%.rds

${DATADIR}/sgtf_%.csv: R/sgtf_public.R ${DATADIR}/sgtf_ll_%.rds 
	$(call R)

${INDIR}/sgtf.rds: R/sgtf.R | ${DATADIR}/sgtf_30.csv
	$(call R,$|)

sgtfraw: ${INDIR}/sgtf.rds $(patsubst %,${DATADIR}/sgtf_%.csv,30 60 90)
