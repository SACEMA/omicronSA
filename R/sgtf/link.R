
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
allreinf <- readRDS(.args[2])

reinfwindow <- as.integer(gsub("^.+_(\\d+)\\.rds$", "\\1", basename(tail(.args, 1))))

#' initially, only consider test events since shortly before start of SGTF testing
reinf <- allreinf[
    date >= (sgtf.clean[, min(specreceiveddate) ] - reinfwindow)
][, .SD[inf == max(inf)], by=caseid_hash ]

join.dt <- reinf[sgtf.clean, on=.(caseid_hash), allow.cartesian = TRUE]

others <- join.dt[is.na(inf), unique(caseid_hash)]
find.others <- allreinf[caseid_hash %in% others]

#' found these in older records => reinfections
join.dt[
    is.na(inf) & (caseid_hash %in% unique(find.others$caseid_hash)),
    inf := 2 # might actually be higher, but irrelevant to follow steps
]

#' the remaining infections represent primary infections
join.dt[is.na(inf), inf := 1]

#' consolidate into
#'  - case id
#'  - infection vs reinfection
#'  - sgtf vs not
#'  - date == earliest date in testing / infection episode (may be earlier than SGTF test itself)
res.dt <- join.dt[
    is.na(specreceiveddate) | (specreceiveddate <= i.specreceiveddate),
    .(
        specreceiveddate = fcoalesce(min(specreceiveddate), i.specreceiveddate[1]),
        sgtfdate = i.specreceiveddate[1],
        sgtf = max(sgtf), # if any sgtf == 1 in testing episode, count as sgtf == 1
        province = fcoalesce(province, i.province)[1],
        publicprivateflag = publicprivateflag[1]
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

res.dt[, prov := names(regionkey)[which(province == regionkey)], by=.(province)]

saveRDS(res.dt, tail(.args, 1))
