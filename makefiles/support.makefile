
# use in rule as $(call R) or $(call R,${SOMETHING}) to insert atypical arguments
R = $(strip Rscript $^ $(1) $@)
# copies first dependency and order-only dependencies; intended to be used with
# one dependency XOR one order-only dependency XOR one argument
# other uses caveat emptor
cp = $(strip cp $< $| $(1) $@)
# creates a symbolic link
ln = ln -s $(abspath $(1)) $(2)


# TODO consolidate these; 2nd is specialization of 1st
define cpgen =
.PRECIOUS: $(1)
$(1): | $(2)
	cp $$| $$@

endef

define lngen =
.PRECIOUS: $(1)
$(1): | $(2)
	$$(call ln,$$|,$$@)

endef

define cptar =
.PRECIOUS: ${DATADIR}/$(1)
${DATADIR}/$(1): ${DLDIR}/$(1)
	cp $$< $$@

endef

# use as $(call WGET,${SOMEURL})
WGET = wget -c -O $@ $(1)

${LNDIR}: | ${REFDIR}
	$(call ln,$|,$@)

${MKDIRS}: | ${LNDIR}
	mkdir -p $@

dirs: ${MKDIRS}
