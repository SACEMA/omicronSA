
# use in rule as $(call R) or $(call R,${SOMETHING}) to insert atypical arguments
R = $(strip Rscript $^ $(1) $@)
# use as $(call WGET,${SOMEURL})
WGET = wget -c -O $@ $(1)

${ROOTDIR}: | ${LNDIR}
	ln -s $| $@

${MKDIRS}: | ${ROOTDIR}
	mkdir $@

dirs: ${MKDIRS}
