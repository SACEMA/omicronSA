
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c(30, 60, 90)[3]
.args <- if (interactive()) c(
    file.path("refdata", sprintf("prov_ts_%i.RDS", .debug)),
    file.path("analysis", "input", sprintf("incidence_%i.rds", .debug))
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
