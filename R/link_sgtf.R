
suppressPackageStartupMessages({
    require(data.table)
    require(haven)
})

.args <- if (interactive()) { c(
    file.path("refdata", "pos_test_ll_90.RDS"),
    file.path("refdata", "sgtf_list_anon_20211220_updated.dta"),
    file.path("refdata", "sgtf_ll.rds")
) } else commandArgs(trailingOnly = TRUE)

sgtf <- as.data.table(read_dta(.args[2]))[,
    .(
        caseid_hash,
        province = as.character(province),
        publicprivateflag = as.factor(publicprivateflag),
        ct30 = as.logical(as.integer(ct30)),
        # note: only ct30 == TRUE results in inputs
        sgtf = as.integer(sgtf),
        specreceiveddate = as.Date(specreceiveddate),
        specreportdate = as.Date(specreportdate),
        speccollectiondate = as.Date(speccollectiondate)
    )
]

# make same date adjustment as in reinfections work
sgtf[
    (specreceiveddate == specreportdate) & ((speccollectiondate + 7) <= specreceiveddate),
    specreceiveddate := speccollectiondate
]

sgtf.clean <- setkey(sgtf[!is.na(sgtf)], province, specreceiveddate)

if (sgtf[,.N] != sgtf.clean[,.N]) {
    warning("Removing records with NA sgtf results: ")
    warning(sprintf(
        "%i caseid_hashes out of %i records:",
        sgtf[is.na(sgtf), .N],
        sgtf[, .N]
    ))
    warning(sgtf[is.na(sgtf), paste(caseid_hash, collapse ="\n")])
}

# initially, only consider test events since shortly before start of SGTF testing
reinf <- readRDS(.args[1])[ date >= (sgtf.clean[, min(specreceiveddate) ]-7) ]

#' TODO appears to be 3 missing records - run down?
join.dt <- reinf[sgtf.clean, on=.(caseid_hash), allow.cartesian = TRUE]

others <- join.dt[is.na(inf), unique(caseid_hash)]
find.others <- readRDS(.args[1])[caseid_hash %in% others]

join.dt[
    is.na(inf) & (caseid_hash %in% unique(find.others$caseid_hash)),
    inf := 2 # might actually be higher, but irrelevant to follow steps
]

join.dt[is.na(inf), inf := 1]

res.dt <- join.dt[,.(
    specreceiveddate = fcoalesce(min(specreceiveddate), min(i.specreceiveddate)),
    # treat the first test in this infection episode as the receipt date of SGTF
    # even if it was a later test in this episode
    sgtf = max(sgtf) # if any sgtf == 1, will count as sgtf == 1
), by=.(
    caseid_hash,
    province = fcoalesce(province, i.province),
    inf, publicprivateflag
)]

regionkey = c(
    EC="EASTERN CAPE",
    FS="FREE STATE",
    GP="GAUTENG",
    KZN="KWAZULU-NATAL",
    LP="LIMPOPO",
    MP="MPUMALANGA",
    NC="NORTHERN CAPE",
    NW="NORTH WEST",
    WC="WESTERN CAPE",
    ALL="ALL"
)

res.dt[, prov := names(regionkey)[which(province == regionkey)], by=.(province)]

saveRDS(res.dt, tail(.args, 1))
