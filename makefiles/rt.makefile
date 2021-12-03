
ESTDIR := ${OUTDIR}/omicron_ratios

${ESTDIR}/omicron: 
	Rscript R/est_rt_ratios.R ${INS} ${ESTDIR} omicron

${ESTDIR}/omicronlow: 
	Rscript R/est_rt_ratios.R ${INS} ${ESTDIR} omicronlow

${ESTDIR}/delta: 
	Rscript R/est_rt_ratios.R ${INS} ${ESTDIR} delta

${OUTDIR}/omicron_ratios.rds: R/consolidate.R ${OUTDIR}/omicron_ratios
	$(call R)

${FIGDIR}/omicron_ratios.png: R/rt_ratios.R ${OUTDIR}/omicron_ratios.rds
	$(call R)
