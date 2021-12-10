
suppressPackageStartupMessages({
    require(data.table)
    require(haven)
})

.args <- if (interactive()) { c(
    file.path("analysis", "input", "pos_test_ll_90.RDS"),
    file.path("refdata", "sgtf_list_anon_20211209.dta"),
    file.path("refdata", "sgtf_ll.rds")
) } else commandArgs(trailingOnly = TRUE)

sgtf <- as.data.table(read_dta(.args[2]))[,
    .(caseid_hash, province, publicprivateflag, ct30, sgtf, specreceiveddate)
]
reinf <- readRDS(.args[1])[ date >= sgtf[, min(specreceiveddate)] ]

#' TODO appears to be 3 missing records - run down?

join.dt <- reinf[sgtf, on=.(caseid_hash), allow.cartesian = TRUE]
res.dt <- join.dt[,.(
    specreceiveddate = min(specreceiveddate), sgtf = max(sgtf)
), by=.(caseid_hash, province, inf, publicprivateflag)]

saveRDS(res.dt, tail(.args, 1))
