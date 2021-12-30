
suppressPackageStartupMessages({
    require(data.table)
    require(haven)
})

.debug <- c(30, 60, 90)[1]
.args <- if (interactive()) { c(
    file.path("refdata", sprintf("pos_test_ll_%i.RDS", .debug[1])),
    file.path("refdata", "sgtf_list_anon_20211220_updated.dta"),
    file.path("refdata", sprintf("sgtf_ll_%i.rds", .debug[1]))
) } else commandArgs(trailingOnly = TRUE)

warn <- function(ws, to = stderr()) invisible(sapply(ws, function(w) write(w, file = to)))

sgtf <- as.data.table(read_dta(.args[2]))[,
    .(
        caseid_hash,
        province = as.character(province),
        publicprivateflag = as.factor(publicprivateflag),
        ct30 = as.logical(as.integer(ct30)),
        #' note: only ct30 == TRUE results in inputs
        sgtf = as.integer(sgtf),
        specreceiveddate = as.Date(specreceiveddate),
        specreportdate = as.Date(specreportdate),
        speccollectiondate = as.Date(speccollectiondate)
    )
]

sgtf.clean <- setkey(sgtf[!is.na(sgtf)], province, specreceiveddate)

if (sgtf[,.N] != sgtf.clean[,.N]) {
    warn(c(
        "WARN: Removing records with NA sgtf results.",
        sprintf(
        "%i caseid_hashes out of %i records:", sgtf[is.na(sgtf), .N], sgtf[, .N]
        ),
        sgtf[is.na(sgtf)][order(specreceiveddate), paste(caseid_hash, specreceiveddate, sep = " on ", collapse ="\n")]
    ))
}

#' make same date adjustment as in reinfections work:
#' if the specimen receipt date and report date are the same,
#' and are more than a week later than the collection date,
#' set the receive date (which is used as the reference date)
#' to the collection date
#' 
#' n.b. assumes there are no receipt dates or report dates that are NA
correctingview <- expression(
    (specreceiveddate == specreportdate) &
    (as.numeric(specreceiveddate - speccollectiondate) > 7)
)
if (sgtf.clean[eval(correctingview), .N]) {
    cview <- sgtf[eval(correctingview)]
    warn(c(
        sprintf(
            "WARN: %i records have specimen receipt dates > specimen collection date + 7.",
            cview[,.N]
        ),
        "The following caseid_hash will have specimen receipt date set to collection date:",
        cview[, paste(caseid_hash, " : ", specreceiveddate, " => ", speccollectiondate, sep = "", collapse ="\n")]
    ))
}

sgtf.clean[
    eval(correctingview), specreceiveddate := speccollectiondate
]

allreinf <- readRDS(.args[1])
#' initially, only consider test events since shortly before start of SGTF testing
reinf <- allreinf[ date >= (sgtf.clean[, min(specreceiveddate) ]-7) ]

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
][, .SD[.N], by=caseid_hash ]

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
