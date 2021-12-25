
# define focal periods for estimation, by province
${INDIR}/timing.rds: R/timing.R | ${INDIR}
	$(call R)

# TODO define target to fetch reinfections series from preprint data sharing
${INDIR}/incidence.rds: R/incidence.R ${INDIR}/prov_ts_90.RDS
	$(call R)

${INDIR}/%/incidence_ensemble.rds: R/ensemble.R ${INDIR}/incidence.rds ${OUTDIR}/sgtf/%/sims.rds
	$(call R)

RAWSGTF ?= $(shell ls -t ${DATADIR}/sgtf_list_anon_*.dta | head -1)

${DATADIR}/sgtf_ll.rds: R/link_sgtf.R ${DATADIR}/pos_test_ll_90.RDS ${RAWSGTF}
	$(call R)

${DATADIR}/sgtf.csv: R/sgtf_public.R ${DATADIR}/sgtf_ll.rds 
	$(call R)

${INDIR}/sgtf.rds: R/sgtf.R | ${DATADIR}/sgtf.csv
	$(call R,$|)

sgtf: ${INDIR}/sgtf.rds