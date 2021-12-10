
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(scales)
})

.args <- if (interactive()) file.path(
    "analysis",
    c("input", "output"),
    c("prov_ts_90.RDS", "incidence.rds")
) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])

#' N.B. date == specimen_receipt_date
setnames(dt,
    c("cnt", "more_than_3", "reinf", "third"),
    c("inf1", "inf4", "inf2", "inf3")
)

dt[, tot := inf1 + inf2 + inf3 + inf4, by=province ]

res.dt <- rbind(
    dt[, .(date, province, tot, inf1)],
    dt[, .(province = "ALL", tot = sum(tot), inf1 = sum(inf1)), by=date]
)

saveRDS(res.dt, tail(.args, 1))