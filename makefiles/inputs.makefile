
# define focal periods for estimation, by province
${INDIR}/timing.rds: R/timing.R | ${INDIR}
	$(call R)

# TODO define target to fetch reinfections series from preprint data sharing
${INDIR}/incidence.rds: R/incidence.R ${INDIR}/prov_ts_90_pub.rds
	$(call R)

${INDIR}/incidence_ensemble.rds: R/ensemble.R ${INDIR}/incidence.rds ${INDIR}/sssims.rds
	$(call R)

# ML/JD TODO: fill in rule
# ${INDIR}/sgtf.sims.rds: R/frequency.R ${INDIR}/SGTF.csv
#	$(call R)

INS := $(patsubst %,${INDIR}/%.rds,timing incidence frequencies)
