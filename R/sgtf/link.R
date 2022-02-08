
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c(30, 60, 90)[3]
.args <- if (interactive()) c(
    file.path("refdata", "sgtf_list_anon.rds"),
    file.path("refdata", sprintf(c(
        "pos_test_ll_%i.RDS",
        "sgtf_ll_%i.rds"
    ), .debug[1]))
) else commandArgs(trailingOnly = TRUE)

sgtf.clean <- readRDS(.args[1])
allreinf <- readRDS(.args[2])[, .(caseid_hash, date, province, pos_test, days, inf)]

reinfwindow <- as.integer(gsub("^.+_(\\d+)\\.rds$", "\\1", basename(tail(.args, 1))))

#' initially, only consider test events since shortly before start of SGTF testing
reinf <- allreinf[
    date >= (sgtf.clean[, min(date) ] - reinfwindow)
]

sgtf.clean[, cutoff := c(as.Date(NA), date[-.N]), by = caseid_hash]
sgtf.clean[ (date - cutoff) > reinfwindow, cutoff := date - reinfwindow ]

join.dt <- reinf[
	sgtf.clean, on=.(caseid_hash), allow.cartesian = TRUE
][
	date <= i.date
][
	is.na(cutoff) | (date > cutoff)
]

others <- join.dt[is.na(inf), unique(caseid_hash)]
find.others <- allreinf[caseid_hash %in% others, unique(caseid_hash)]

#' found these in older records => reinfections
join.dt[
    is.na(inf),
    # might actually be higher than inf 2, but irrelevant to follow steps
    inf := fifelse(caseid_hash %in% find.others, 2, 1)
]

#' consolidate into
#'  - case id
#'  - infection vs reinfection
#'  - sgtf vs not
#'  - date == earliest date in testing / infection episode (may be earlier than SGTF test itself)
res.dt <- join.dt[
    is.na(date) | (date <= i.date),
    .(
        date = fcoalesce(min(date), i.date[1]),
        sgtfdate = i.date[1],
        sgtf,
        province = fcoalesce(province, as.character(i.province[1]))
    ),
    keyby=.(
        caseid_hash, inf
    )
]



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

res.dt[, prov := factor(
	names(regionkey)[which(province == regionkey)],
	names(regionkey),
	ordered = TRUE
), by=.(province)]

saveRDS(res.dt, tail(.args, 1))
