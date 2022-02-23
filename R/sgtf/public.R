
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c(30, 60, 90, "trim", "hold")[1]
.args <- if (interactive()) c(
    file.path("refdata", sprintf(c("sgtf_ll_%s.rds", "sgtf_%s.csv"), .debug[1]))
) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])[, infection := fifelse(reinf, "reinfection", "primary") ]

res <- dcast(dt, prov + date + infection ~ sgtf, fun.aggregate = length)

setnames(res, c("0", "1"), c("nonSGTF", "SGTF"))
setkey(res, prov, date, infection)

fwrite(res, tail(.args, 1))
