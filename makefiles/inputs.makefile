
# define focal periods for estimation, by province
${INDIR}/timing.rds: R/timing.R | ${INDIR}
	$(call R)

# TODO define target to fetch reinfections series from preprint data sharing
${INDIR}/incidence.rds: R/incidence.R ${INDIR}/prov_ts_90.RDS
	$(call R)

${INDIR}/incidence_ensemble.rds: R/ensemble.R ${INDIR}/incidence.rds
	$(call R)

${DATADIR}/sgtf_ll.rds: R/link_sgtf.R ${INDIR}/pos_test_ll_90.RDS ${DATADIR}/sgtf_list_anon_20211209.dta
	$(call R)

${DATADIR}/sgtf.rds: R/sgtf.R ${DATADIR}/sgtf_ll.rds
	$(call R)

