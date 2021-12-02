
GMURL := https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv
OXBASEURL := https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c1_%.csv
OXSCHOOLS := $(patsubst %,${OXBASEURL},school_closing)
OXSCHFLAG := $(patsubst %,${OXBASEURL},flag)

${INDIR}/mobility.csv:
	$(call WGET,${GMURL})

${INDIR}/schools.csv:
	$(call WGET,${OXSCHOOLS})

# n.b. not actually used for SA
${INDIR}/schflag.csv:
	$(call WGET,${OXSCHFLAG})

MOB := ${OUTDIR}/mobility.rds

${MOB}: mobility.R $(patsubst %,${INDIR}/%.csv,mobility schools schflag)
	$(call R)

mobility: ${MOB}

# TODO: mobility figure, though not a priority focus