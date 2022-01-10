
# use in rule as $(call R) or $(call R,${SOMETHING}) to insert atypical arguments
R = $(strip Rscript $^ $(1) $@)
# copies first dependency and order-only dependencies; intended to be used with
# one dependency XOR one order-only dependency XOR one argument
# other uses caveat emptor
cp = $(strip cp $< $| $(1) $@)

# TODO consolidate these; 2nd is specialization of 1st
define cpgen =
.PRECIOUS: $(1)
$(1): | $(2)
	cp $$| $$@

endef

define cptar =
.PRECIOUS: ${DATADIR}/$(1)
${DATADIR}/$(1): ${DLDIR}/$(1)
	cp $$< $$@

endef

# use as $(call WGET,${SOMEURL})
WGET = wget -c -O $@ $(1)

${LNDIR}: | ${REFDIR}
	ln -s $| $@

${MKDIRS}: | ${LNDIR}
	mkdir -p $@

dirs: ${MKDIRS}
