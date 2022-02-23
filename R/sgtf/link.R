
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

#' assert: link.R provided source data labelled according to reinfection threshold
#' TODO: make explicit argument rather than file name?
reinfwindow <- as.integer(gsub("^.+_(\\d+)\\.rds$", "\\1", basename(tail(.args, 1))))

sgtf.clean <- readRDS(.args[1])[is.na(threshold) | threshold == reinfwindow]
#' use same short.threshold as import
short.threshold <- 15
#' set limits for rewinding episodes split based on SG* status
sgtf.clean[,
  cutoff := if (.N == 1) date[1]-reinfwindow else c(
  	date[1]-reinfwindow, pmax((date + short.threshold)[-.N], date[-1]-reinfwindow)),
  by=caseid_hash
]

allreinf <- readRDS(.args[2])[, .(caseid_hash, date, province, pos_test, days, inf)]

#' for join efficiency, only consider test events within max rewind SGTF window
reinf <- allreinf[
    date >= (sgtf.clean[, min(date) ] - reinfwindow)
]

join.dt <- reinf[
	sgtf.clean, on=.(caseid_hash), allow.cartesian = TRUE
][
	is.na(date) | between(date, cutoff, i.date)
]

others <- join.dt[is.na(date), unique(caseid_hash)]
find.others <- allreinf[caseid_hash %in% others, unique(caseid_hash)]

#' found these in older records => reinfections; didn't, assume first infections that are other missed
#' in general test+ ll
join.dt[
    is.na(inf),
    #' might actually be higher than inf 2, but irrelevant to follow steps
    inf := fifelse(caseid_hash %in% find.others, 2L, 1L)
]

#' consolidate into
#'  - case id
#'  - infection vs reinfection
#'  - sgtf vs not
#'  - date == earliest date in testing / infection episode (may be earlier than SGTF test itself)
res.dt <- join.dt[,
    .(
        date = fcoalesce(min(date), i.date[1]),
        sgtfdate = i.date[1],
        sgtf = sgtf[1],
        province = fcoalesce(province, as.character(i.province[1]))[1],
        reinf = max(inf, episode) != 1
    ),
    keyby=.(
        caseid_hash, episode
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
